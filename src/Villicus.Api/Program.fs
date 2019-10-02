open Villicus
open Villicus.Domain
open Villicus.CommandHandlers
open Villicus.Serialization
#if FABLE_COMPILER
open Thoth.Json
#else
open Thoth.Json.Net
#endif

type APIError =
    | Duplicate of DuplicateWorkflowIdException
    | Unknown of exn
    | NotFound of System.Guid
    | BadRequest of exn
    | InvalidUUID of string

let utf8 = System.Text.Encoding.UTF8

let clientPath = System.IO.Path.Combine("../", "Client") |> System.IO.Path.GetFullPath

let repo = Persistence.MemoryRepository.create<VersionedWorkflowId,Published<WorkflowModel>> ()
let wfDispatcher = 
    let d = Persistence.MemoryEventStore.create<string,WorkflowEvent> () |> Workflow.createDispatcher 0
    Workflow.versionProjection d repo.Save
    d.Agent
let jDispatcher =
    let es = Persistence.MemoryEventStore.create<string,JourneyEvent<System.Guid>> ()
    Journey.createDispatcher 0 es repo


module OldSuave =

    open Suave
    open Suave.Operators
    open Suave.Filters

    let errorContent = Writers.setHeader "Content-Type" "text/plain; charset=UTF-8"

    let processCommand =
        WFCommand.newCommandStream
        >> wfDispatcher.PostAndAsyncReply
        >> Async.map(Result.map2
            (fun (eventIndex,eventList) ->
                (eventList
                |> List.map (WorkflowEvent.Encoder)
                |> Encode.list
                |> Encode.toString 4
                |> Successful.CREATED)     
                >=> (eventIndex |> string |> Writers.setHeader "Etag"))
            (fun error ->
                match error with
                | :? DuplicateWorkflowIdException as e -> Duplicate e
                | _ -> Unknown error))

    let getOptions ctx =
        ctx.request.query
        |> List.choose (fun (k,v) -> v |> Option.map (fun v' -> k.ToLower(),v')) |> Map.ofList

    let getIntOption (name:string) (minVal:int64) (maxVal:int64) (defaultVal:int64) options =
        match options |> Map.tryFind (name.ToLower()) with
        //todo: validate pagesize ?
        | Some (s:string) -> match System.Int64.TryParse(s) with | true,n -> n | _ -> defaultVal
        | None -> defaultVal
        |> fun x -> System.Math.Max(minVal,System.Math.Min(x,maxVal))

    let linkheader ctx offSt pgSize rel =
        let b = System.UriBuilder(ctx.request.url)
        b.Host <- ctx.request.clientHost true []
        b.Query <-
          ctx |> getOptions |> Map.add "offset" (string offSt)
          |> Map.add "pagesize" (string pgSize)
          |> Map.toArray |> Array.map (fun (k,v) -> sprintf "%s=%s" k v)
          |> Array.toSeq |> String.concat "&"
        let uri = b.Uri.ToString()
        sprintf "<%s>; rel=%s" uri rel
        |> Writers.addHeader "Link"

    let pagingHeaders ctx pageSize eventIndex nextEventId =
        let lh = linkheader ctx
        match eventIndex,nextEventId with
        | 0L,None ->
            //TODO: should we always send link headers for paging in order to be consistent?
            Some >> Async.result // no pages needed, no headers
        | 0L,Some nextId | _,Some nextId ->
            let priorId = System.Math.Max(0L,eventIndex - (int64 pageSize))
            lh 0L pageSize "first" >=>
            lh priorId pageSize "prev" >=>
            lh nextId pageSize "next"
        | _,None -> 
            let priorId = System.Math.Max(0L,eventIndex - (int64 pageSize))
            lh 0L pageSize "first" >=>
            lh priorId pageSize "prev"

    let getEvents wfid : (HttpContext -> Async<Result<WebPart,APIError>>) = fun (ctx:HttpContext) ->
        let options = getOptions ctx
        let pageSize = 
            let pgSize = getIntOption "pagesize" 10L 100L 50L options
            System.Convert.ToInt32(pgSize) // don't need to catch exn because we know it is in range 10-50
        let offSet = options |> getIntOption "offset" 0L System.Int64.MaxValue 0L
        let postMsg = WFCommand.newReadStream (WorkflowId wfid) offSet pageSize
        wfDispatcher.PostAndAsyncReply postMsg
        |> Async.map(
            Result.mapError Unknown
            >> Result.bind(fun (eventList,eventIndex,nextEventId) ->
                match eventList.Length,offSet with
                | 0,0L -> wfid |> NotFound |> Error
                | _ ->
                  Ok(
                    eventList
                    |> List.map WorkflowEvent.Encoder
                    |> Encode.list
                    |> Encode.toString 4
                    |> Successful.OK
                    >=> pagingHeaders ctx pageSize eventIndex nextEventId)))

    let sendCommand : HttpContext -> Async<Result<WebPart,APIError>> = fun (ctx:HttpContext) ->
        ctx.request.rawForm |> utf8.GetString |> Decode.fromString WorkflowCommand.Decoder
        |> function
            | Ok x -> processCommand x
            | Error e -> e |> exn |> BadRequest |> Error |> Async.result

    let appendIdandSend (wfid:System.Guid) = fun (ctx:HttpContext) ->
        let o =
            ctx.request.rawForm |> utf8.GetString |> Newtonsoft.Json.Linq.JObject.Parse :> Newtonsoft.Json.Linq.JToken
        o.["workflowId"] <- Newtonsoft.Json.Linq.JValue(wfid.ToString())
        WorkflowCommand.Decoder "" o
        |> function
            | Ok x -> processCommand x
            | Error err -> err.ToString() |> exn |> BadRequest |> Error |> Async.result

    let getWorkflow wfid = fun (ctx:HttpContext) ->
        Common.getState (WorkflowId wfid) 0L
        |> wfDispatcher.PostAndAsyncReply
        |> Async.map(
            Result.mapError Unknown
            >> Result.bind(function
                | (_,None) -> wfid |> NotFound |> Error
                | (eventIndex,Some wfModel) ->
                  Ok(
                    wfModel |> WorkflowModel.Encoder |> Encode.toString 4 |> Successful.OK
                    >=> Writers.setHeader "Etag" (string eventIndex))))

    let formatErrors (f: HttpContext -> Async<Result<WebPart,APIError>>) = fun (ctx:HttpContext) ->
        f ctx
        |> Async.bind(
            Result.mapError(
               function
                | Duplicate e -> RequestErrors.UNPROCESSABLE_ENTITY e.Message
                | Unknown e -> ServerErrors.INTERNAL_ERROR e.Message
                | NotFound guid -> guid.ToString() |> sprintf "WorkflowId '%s' not found" |> RequestErrors.NOT_FOUND
                | BadRequest e -> RequestErrors.BAD_REQUEST e.Message
                | InvalidUUID guidStr -> guidStr |> sprintf "'%s' is not a valid UUID" |> RequestErrors.BAD_REQUEST)
            >> function
                | Ok webPart -> webPart
                | Error errWebPart -> errWebPart >=> errorContent
            >> (fun f' -> f' ctx))

    let checkGuid (f: System.Guid -> (HttpContext -> Async<Result<WebPart,APIError>>)) (guidStr:string) =
        printfn "requested id %s" guidStr
        match System.Guid.TryParse guidStr with
        | true,guid -> f guid
        | false,_ -> fun _ -> guidStr |> InvalidUUID |> Error |> Async.result

    let app : WebPart =
      Writers.setHeader "Content-Type" "application/json; charset=UTF-8" >=>
      choose
        [ GET >=> choose
            [ path "/" >=> Files.file "index.html"
              path "" >=> Files.file "index.html"
              pathScan "/api/workflow/%s/events" (checkGuid getEvents >> formatErrors)
              pathScan "/api/workflow/%s" (checkGuid getWorkflow >> formatErrors)
              RequestErrors.NOT_FOUND "Invalid Uri Path" ]
          POST >=> choose
            [ path "/api/workflow" >=> (formatErrors sendCommand)
              pathScan "/api/workflow/%s" (checkGuid appendIdandSend >> formatErrors)
              RequestErrors.NOT_FOUND "Invalid Uri Path" ]
        ]
  

    // default is to run on all local external interfaces
    let startConfig = 
      { defaultConfig with
          homeFolder = Some (System.IO.Path.GetFullPath "../deploy")      
          bindings =
              let defaultHost = (List.head defaultConfig.bindings).socketBinding.ip
              System.Net.Dns.GetHostAddresses(System.Net.Dns.GetHostName())
              |> Array.append [| defaultHost |]
              |> Array.map (fun ip -> HttpBinding.create HTTP ip 8085us)
              |> Array.toList }
          //[ HttpBinding.createSimple HTTP "10.0.1.34" 9000 ] }

module NewSaturn =

    open Saturn
    open Saturn.Pipeline
    open Giraffe
    open Giraffe.Core
    open Giraffe.ResponseWriters
    open Microsoft.Extensions.DependencyInjection
    open FSharp.Control.Tasks.V2.ContextInsensitive
    open Microsoft.AspNetCore.Http
    open System.Threading.Tasks

    let getOptions (ctx:HttpContext) =
        ctx.Request.Query
        |> Seq.map (fun kvp -> kvp.Value |> Seq.head |> (fun v' -> (kvp.Key.ToLower(),v'))) |> Map.ofSeq

    let getIntOption (name:string) (minVal:int64) (maxVal:int64) (defaultVal:int64) options =
        match options |> Map.tryFind (name.ToLower()) with
        //todo: validate pagesize ?
        | Some (s:string) -> match System.Int64.TryParse(s) with | true,n -> n | _ -> defaultVal
        | None -> defaultVal
        |> fun x -> System.Math.Max(minVal,System.Math.Min(x,maxVal))

    let linkheader offSt pgSize rel = fun (next:HttpFunc) (ctx:HttpContext) ->
        let b = System.UriBuilder(Extensions.UriHelper.GetDisplayUrl ctx.Request)
        b.Host <- ctx.Request.Host.Host
        b.Query <-
          ctx |> getOptions |> Map.add "offset" (string offSt)
          |> Map.add "pagesize" (string pgSize)
          |> Map.toArray |> Array.map (fun (k,v) -> sprintf "%s=%s" k v)
          |> Array.toSeq |> String.concat "&"
        let uri = b.Uri.ToString()
        sprintf "<%s>; rel=%s" uri rel
        |> ctx.SetHttpHeader "Link"
        next ctx

    let pagingHeaders pageSize eventIndex nextEventId = fun (nextFunc:HttpFunc) (ctx:HttpContext) ->
        let priorId = System.Math.Max(0L,eventIndex - (int64 pageSize))
        let prev = linkheader priorId pageSize "prev"
        let first = linkheader 0L pageSize "first"
        let next nextId = linkheader nextId pageSize "next"
        match eventIndex,nextEventId with
        | (0L,None) -> first
        | (_,Some nextId) -> first >=> prev >=> next nextId
        | (_,None) -> first >=> prev
        |> (fun f -> f nextFunc ctx)

    module Task =
        let inline map (f:'a -> 'b) (x:Task<'a>) = task {
            let! x' = x
            return f x' }
        let inline bind (f: 'a -> Task<'b>) (x: Task<'a>) = task {
            let! x' = x
            return! f x' }

    let processCommand =
        WFCommand.newCommandStream
        >> wfDispatcher.PostAndAsyncReply
        >> Async.StartAsTask
        >> Task.map(Result.map2
            (fun (eventIndex,eventList) ->
                (eventList
                |> List.map (WorkflowEvent.Encoder)
                |> Encode.list
                |> Encode.toString 4
                |> Successful.CREATED)     
                >=> (eventIndex |> string |>setHttpHeader "Etag"))
            (fun error ->
                match error with
                | :? DuplicateWorkflowIdException as e -> Duplicate e
                | _ -> Unknown error))

    let postText (ctx:HttpContext) =
        use body = new System.IO.StreamReader(ctx.Request.Body)
        //The modelbinder has already read the stream and need to reset the stream index ???
        // body.BaseStream.Seek(0L, System.IO.SeekOrigin.Begin) |> ignore
        body.ReadToEnd ()

    let sendCommand : HttpContext -> Task<Result<HttpHandler,APIError>> = fun (ctx:HttpContext) ->
        ctx |> postText |> Decode.fromString WorkflowCommand.Decoder
        |> function
            | Ok x -> processCommand x
            | Error e -> e |> exn |> BadRequest |> Error |> Task.FromResult

    let appendIdandSend (wfid:System.Guid) = fun (ctx:HttpContext) ->
        let o =
            ctx |> postText |> Newtonsoft.Json.Linq.JObject.Parse :> Newtonsoft.Json.Linq.JToken
        o.["workflowId"] <- Newtonsoft.Json.Linq.JValue(wfid.ToString())
        WorkflowCommand.Decoder "" o
        |> function
            | Ok x -> processCommand x
            | Error err -> err.ToString() |> exn |> BadRequest |> Error |> Task.FromResult

    let getEvents wfid : (HttpContext -> Task<Result<HttpHandler,APIError>>) = fun (ctx:HttpContext) ->
        let options = getOptions ctx
        let pageSize = 
            let pgSize = getIntOption "pagesize" 10L 100L 50L options
            System.Convert.ToInt32(pgSize) // don't need to catch exn because we know it is in range 10-50
        let offSet = options |> getIntOption "offset" 0L System.Int64.MaxValue 0L
        let postMsg = WFCommand.newReadStream (WorkflowId wfid) offSet pageSize
        wfDispatcher.PostAndAsyncReply postMsg
        |> Async.StartAsTask
        |> Task.map(
            Result.mapError Unknown
            >> Result.bind(fun (eventList,eventIndex,nextEventId) ->
                match eventList.Length,offSet with
                | 0,0L -> wfid |> NotFound |> Error
                | _ ->
                  Ok(
                    eventList
                    |> List.map WorkflowEvent.Encoder
                    |> Encode.list
                    |> Encode.toString 4
                    |> Successful.OK
                    >=> pagingHeaders pageSize eventIndex nextEventId)))

    let getWorkflow wfid = fun (_:HttpContext) ->
        Common.getState (WorkflowId wfid) 0L
        |> wfDispatcher.PostAndAsyncReply
        |> Async.StartAsTask |> Task.map(
            Result.mapError Unknown
            >> Result.bind(function
                | (_,None) -> wfid |> NotFound |> Error
                | (eventIndex,Some wfModel) ->
                  Ok(
                    wfModel |> WorkflowModel.Encoder |> Encode.toString 4 |> Successful.OK
                    >=> setHttpHeader "Etag" (string eventIndex))))

    let formatErrors (f: HttpContext -> Task<Result<HttpHandler,APIError>>) = fun (next:HttpFunc) (ctx:HttpContext) ->
        let errorContent = setHttpHeader "Content-Type" "text/plain; charset=UTF-8"
        ctx |> f |> Task.bind(
            Result.mapError(function
                | Duplicate e -> RequestErrors.UNPROCESSABLE_ENTITY e.Message
                | Unknown e -> ServerErrors.INTERNAL_ERROR e.Message
                | NotFound guid -> guid.ToString() |> sprintf "WorkflowId '%s' not found" |> RequestErrors.NOT_FOUND
                | BadRequest e -> RequestErrors.BAD_REQUEST e.Message
                | InvalidUUID guidStr -> guidStr |> sprintf "'%s' is not a valid UUID" |> RequestErrors.BAD_REQUEST)
            >> function
                | Ok handler -> handler
                | Error errHandler -> (errHandler >=> errorContent)
            >> fun handler -> handler next ctx )

    let checkGuid (f: System.Guid -> (HttpContext -> Task<Result<HttpHandler,APIError>>)) (guidStr:string) =
        printfn "requested id %s" guidStr
        match System.Guid.TryParse guidStr with
        | true,guid -> f guid
        | false,_ -> fun _ -> guidStr |> InvalidUUID |> Error |> Task.FromResult

    let apiRouter = router {
        pipe_through (pipeline {
            set_header "x-pipeline-type" "Api"
            set_header "Content-Type" "application/json; charset=UTF-8" })
        post "/api/workflow" (formatErrors sendCommand)
        postf "/api/workflow/%s" (checkGuid appendIdandSend >> formatErrors)
        getf "/workflow/%s/events" (checkGuid getEvents >> formatErrors)
        getf "/workflow/%s" (checkGuid getWorkflow >> formatErrors) }

    let clientHandler =
        let clientPath = System.IO.Path.Combine("..","Client")|> System.IO.Path.GetFullPath
        htmlFile (System.IO.Path.Combine(clientPath, "/index.html"))

    let port = 8085us

    let browserRouter = router {
        get "/" clientHandler
        get "" clientHandler }

    let mainRouter = router {
        forward "/api" apiRouter
        forward "" browserRouter }

    let config (services:IServiceCollection) =
        services.AddSingleton<Giraffe.Serialization.Json.IJsonSerializer>(Thoth.Json.Giraffe.ThothSerializer())

    let app = application {
        use_router mainRouter
        url ("http://0.0.0.0:" + port.ToString() + "/")
        memory_cache 
        use_static clientPath
        service_config config
        use_gzip }


[<EntryPoint>]
let main argv =

    Saturn.Application.run NewSaturn.app
    0
