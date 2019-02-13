// Learn more about F# at http://fsharp.org

open Suave
open Suave.Operators
open Suave.Filters
open Villicus
open Villicus.Domain
open Villicus.CommandHandlers
open Villicus.Serialization
#if FABLE_COMPILER
open Thoth.Json
#else
open Thoth.Json.Net
#endif

let utf8 = System.Text.Encoding.UTF8


[<EntryPoint>]
let main argv =

    let repo = Persistence.MemoryRepository.create<VersionedWorkflowId,Published<WorkflowModel>> ()
    let wfDispatcher = 
        let d = Persistence.MemoryEventStore.create<string,WorkflowEvent> () |> Workflow.createDispatcher 0
        Workflow.versionProjection d repo.Save
        d
    let jDispatcher =
        let es = Persistence.MemoryEventStore.create<string,JourneyEvent<System.Guid>> ()
        Journey.createDispatcher 0 es repo

    let errorContent = Writers.setHeader "Content-Type" "text/plain; charset=UTF-8"
    let processCommand wfCommand ctx =
        fun r -> wfCommand |> ResultCommandStream.New r |> ResultCommandStream
        |> wfDispatcher.Agent.PostAndAsyncReply
        |> Async.bind(function
            | Ok (eventIndex,eventList) ->
               (eventIndex |> string |> Writers.setHeader "Etag" >=>
                 (eventList |> List.map (WorkflowEvent.Encoder) |> Encode.list
                    |> Encode.toString 4 |> Successful.CREATED)) ctx
            | Error error ->
              (errorContent >=>            
                match error with
                | :? DuplicateWorkflowIdException as e ->
                    //todo - set Location header to the existing workflow?
                    RequestErrors.UNPROCESSABLE_ENTITY e.Message
                | _ -> ServerErrors.INTERNAL_ERROR error.Message) ctx)            

    let sendCommand : WebPart =
      fun (ctx:HttpContext) ->
        ctx.request.rawForm |> utf8.GetString 
        |> Decode.fromString WorkflowCommand.Decoder |> function
          | Error msg -> (errorContent >=> RequestErrors.BAD_REQUEST msg) ctx
          | Ok wfCommand -> processCommand wfCommand ctx

    let appendIdandSend wfidStr : WebPart =
        fun (ctx:HttpContext) ->
          printfn "requested id %s" wfidStr
          match System.Guid.TryParse wfidStr with
            | true,_ ->
                let o = ctx.request.rawForm |> utf8.GetString |> Newtonsoft.Json.Linq.JObject.Parse
                o.["workflowId"] <- Newtonsoft.Json.Linq.JValue(wfidStr)
                o :> Newtonsoft.Json.Linq.JToken |> WorkflowCommand.Decoder "" |> function
                  | Error err -> (errorContent >=> RequestErrors.BAD_REQUEST (err.ToString())) ctx
                  | Ok wfCommand -> processCommand wfCommand ctx
            | _ -> (wfidStr |> sprintf "'%s' is not a valid UUID" |> RequestErrors.BAD_REQUEST >=> errorContent) ctx

    let getWorkflow wfidStr : WebPart =
        fun (ctx:HttpContext) ->
          printfn "requested id %s" wfidStr
          match System.Guid.TryParse wfidStr with
            | true,wfid ->
                wfDispatcher.Agent.PostAndAsyncReply(fun r -> GetState (WorkflowId wfid,0L,r))
                |> Async.bind (function
                    | Ok (eventIndex,wf) ->
                        match wf with
                        | Some wfModel ->
                          (eventIndex |> string |> Writers.setHeader "Etag" >=>
                            (wfModel |> WorkflowModel.Encoder 
                             |> Encode.toString 4 |> Successful.OK)) ctx
                        | None -> 
                            (wfidStr |> sprintf "WorkflowId '%s' not found" |> RequestErrors.NOT_FOUND) ctx
                    | Error error -> 
                        (errorContent >=> ServerErrors.INTERNAL_ERROR error.Message) ctx)            
            | _ -> (wfidStr |> sprintf "'%s' is not a valid UUID" |> RequestErrors.BAD_REQUEST >=> errorContent) ctx

    let getOptions ctx =
        ctx.request.query
        |> List.choose (fun (k,v) -> v |> Option.map (fun v' -> k.ToLower(),v')) |> Map.ofList

    let getIntOption (name:string) minVal maxVal defaultVal options =
        match options |> Map.tryFind (name.ToLower()) with
        //todo: validate pagesize ?
        | Some s -> match System.Int64.TryParse(s) with | true,n -> n | _ -> defaultVal
        | None -> defaultVal
        |> fun x -> System.Math.Max(minVal,System.Math.Min(x,maxVal))


    let getEvents wfidStr : WebPart =
        fun (ctx:HttpContext) ->
          let options = getOptions ctx
          printfn "requested id %s" wfidStr
          match System.Guid.TryParse wfidStr with
            | true,wfid ->
                let pageSize = 
                    options |> getIntOption "pageSize" 10L 100L 50L
                    |> fun x -> System.Convert.ToInt32(x) // don't need to catch exn because we know it is in range 10-50
                let offSet = options |> getIntOption "offset" 0L System.Int64.MaxValue 0L
                wfDispatcher.Agent.PostAndAsyncReply(fun r -> ReadStream.New (WorkflowId wfid) offSet pageSize r |> ReadStream)
                |> Async.bind (function
                    | Ok (eventList,eventIndex,nextEventId) ->
                        match eventList.Length,offSet with
                        | 0,0L ->
                            (wfidStr |> sprintf "WorkflowId '%s' not found" |> RequestErrors.NOT_FOUND
                            >=> errorContent) ctx
                        | _ -> 
                            (eventList |> List.map WorkflowEvent.Encoder |> Encode.list
                            |> Encode.toString 4 |> Successful.OK >=> (
                                let linkheader offSt pgSize rel =
                                    let b = System.UriBuilder(ctx.request.url)
                                    b.Host <- ctx.request.clientHost true []
                                    b.Query <-
                                      options |> Map.add "offset" (string offSt)
                                      |> Map.add "pagesize" (string pgSize)
                                      |> Map.toArray |> Array.map (fun (k,v) -> sprintf "%s=%s" k v)
                                      |> Array.toSeq |> String.concat "&"
                                    let uri = b.Uri.ToString()
                                    sprintf "<%s>; rel=%s" uri rel
                                    |> Writers.addHeader "Link"
                                match eventIndex,nextEventId with
                                | 0L,None -> fun (c:HttpContext) -> c |> Some |> Async.result // no pages needed, no headers
                                | 0L,Some nextId | _,Some nextId ->
                                    let priorId = System.Math.Max(0L,eventIndex - (int64 pageSize))
                                    linkheader 0L pageSize "first" >=>
                                    linkheader priorId pageSize "prev" >=>
                                    linkheader nextId pageSize "next"
                                | _,None -> 
                                    let priorId = System.Math.Max(0L,eventIndex - (int64 pageSize))
                                    linkheader 0L pageSize "first" >=>
                                    linkheader priorId pageSize "prev")) ctx
                    | Error error -> 
                        (errorContent >=> ServerErrors.INTERNAL_ERROR error.Message) ctx)            
            | _ -> (wfidStr |> sprintf "'%s' is not a valid UUID" |> RequestErrors.BAD_REQUEST >=> errorContent) ctx
    let app : WebPart =
      Writers.setHeader "Content-Type" "application/json; charset=UTF-8" >=>
      choose
        [ GET >=> choose
            [ Filters.pathScan "/workflow/%s/events" (fun w -> getEvents w) 
              Filters.pathScan "/workflow/%s" getWorkflow
              RequestErrors.NOT_FOUND "Invalid Uri Path" ]
          POST >=> choose
            [ path "/workflow" >=> sendCommand
              Filters.pathScan "/workflow/%s" appendIdandSend
              RequestErrors.NOT_FOUND "Invalid Uri Path" ]
        ]
  
    // default is to run on all local external interfaces
    let startConfig = 
      { defaultConfig with 
          bindings = 
              System.Net.Dns.GetHostAddresses(System.Net.Dns.GetHostName())
              |> Array.map (fun ip -> HttpBinding.create HTTP ip HttpBinding.DefaultBindingPort)
              |> Array.toList }          
          //[ HttpBinding.createSimple HTTP "10.0.1.34" 9000 ] }

    startWebServer startConfig app
    0
