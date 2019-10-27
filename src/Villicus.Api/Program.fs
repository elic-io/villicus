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
    | WorkflowError of WorkflowError
    | Unknown of exn
    | NotFound of System.Guid
    | BadRequest of exn
    | InvalidUUID of string



let storeNames save =
    function
    | WorkflowRenamed e -> Some (e.WorkflowId,e.Name)
    | WorkflowCreated e -> Some (e.WorkflowId,e.Name)
    | _ -> None
    >> Option.iter (fun (a,b) -> save a b |> ignore)

let repo = Persistence.MemoryRepository.create<VersionedWorkflowId,Published<WorkflowModel>> ()
let workFlowByNameRepo = Persistence.MemoryRepository.create<WorkflowId,string> ()
let wfDispatcher = 
    let dispatcher = Persistence.MemoryEventStore.create<string,WorkflowEvent> () |> Workflow.createDispatcher 0
    Workflow.versionProjection dispatcher repo.Save
    Observable.add (storeNames workFlowByNameRepo.Save) dispatcher.Observable
    dispatcher.Agent

let jDispatcher =
    let es = Persistence.MemoryEventStore.create<string,JourneyEvent<System.Guid>> ()
    Journey.createDispatcher 0 es repo


open Saturn
// open Saturn.Pipeline
open Giraffe
// open Giraffe.Core
// open Giraffe.ResponseWriters
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
    Result.map (
        WFCommand.newCommandStream
        >> wfDispatcher.PostAndAsyncReply
        >> Async.StartAsTask
        >> Task.map(
          Result.map2
            (fun (eventIndex,eventList) ->
                (eventList
                |> List.map (WorkflowEvent.Encoder)
                |> Encode.list
                |> Encode.toString 4
                |> Successful.CREATED)     
                >=> (eventIndex |> string |> setHttpHeader "Etag"))
            WorkflowError))
    >> function
    | Ok x -> x
    | Error e -> e |> CommandCreationError.ToExn |> BadRequest |> Error |> Task.FromResult

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
        Result.mapError WorkflowError
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


let getAllWorkflows : (HttpContext -> Task<Result<HttpHandler,APIError>>) = fun (ctx:HttpContext) ->
    //todo: paged reading of seq needs to move to some Seq lib/module?
    //todo: MemoryStore could (should?) be refactored to use this same function
    //todo: this could probably be refactored to use mutables internally and get everything with one pass on the seq
    let readPage offSet count stream =
        let takeUpTo mx = 
            match mx with 
            | 0 -> id 
            | _ -> Seq.indexed >> Seq.takeWhile(fun (i,_) -> i < mx) >> Seq.map snd
        let iStream =
            Seq.indexed stream
            |> match offSet with
                | 0L -> id
                | _ -> Seq.skip (offSet - 1L |> int)
        let indexedItems = iStream |> takeUpTo count
        let items = indexedItems |> Seq.map snd |> List.ofSeq
        let lastItemNumber = indexedItems |> Seq.last |> fst |> (+) 1
        let nextItemNumber =
            let nextItemCandidate = iStream |> takeUpTo (count+1) |> Seq.last |> fst |> (+) 1
            match lastItemNumber = nextItemCandidate with
                | true -> None
                | false -> nextItemCandidate |> int64 |> Some
        (items, int64 lastItemNumber, nextItemNumber)


    try
        let options = getOptions ctx
        let pageSize = 
            let pgSize = getIntOption "pagesize" 10L 100L 50L options
            System.Convert.ToInt32(pgSize) // don't need to catch exn because we know it is in range 10-50
        let offSet = options |> getIntOption "offset" 0L System.Int64.MaxValue 0L
        workFlowByNameRepo.RetrieveItems (fun _ _ -> true)
        |> Async.StartAsTask
        |> Task.map(fun allWorkflows ->
            let (eventList,itemIndex,nextItemId) =
                allWorkflows
                |> Map.toSeq
                |> readPage offSet pageSize
            eventList
            |> List.map (fun (wfid,name) -> { Api.ViewTypes.WorkflowMetaListItem.WorkflowId = wfid; Api.ViewTypes.WorkflowMetaListItem.Name = name } |> Api.ViewTypes.WorkflowMetaListItem.Encoder )
            |> Encode.list
            |> Encode.toString 4
            |> Successful.OK
            >=> pagingHeaders pageSize itemIndex nextItemId
            |> Ok )
    with e -> e |> Unknown |> Error |> Task.FromResult


let getWorkflow wfid = fun (_:HttpContext) ->
    Common.getState (WorkflowId wfid) 0L
    |> wfDispatcher.PostAndAsyncReply
    |> Async.StartAsTask |> Task.map(
        Result.mapError WorkflowError
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
            | Unknown e -> ServerErrors.INTERNAL_ERROR e.Message
            | NotFound guid -> guid.ToString() |> sprintf "WorkflowId '%s' not found" |> RequestErrors.NOT_FOUND
            | BadRequest e -> RequestErrors.BAD_REQUEST e.Message
            | InvalidUUID guidStr -> guidStr |> sprintf "'%s' is not a valid UUID" |> RequestErrors.BAD_REQUEST
            | WorkflowError wfe ->
                match wfe with
                | CommandCreation ccError ->
                    match ccError with
                    | NullArgument errMsg -> errMsg
                    | CantTargetSelf errMsg -> errMsg
                    |> RequestErrors.BAD_REQUEST
                | MaxCountExceeded maxCountError ->
                    RequestErrors.UNPROCESSABLE_ENTITY maxCountError.Message
                | Duplicate workflowId ->
                    workflowId.ToString ()
                    |> sprintf "A workflow with id '%s' already exists"
                    |> RequestErrors.UNPROCESSABLE_ENTITY
                | NonExistant ->
                    RequestErrors.UNPROCESSABLE_ENTITY "Workflow must be created before processing other commands"
                | WorkflowError.NotFound workflowId -> 
                    workflowId.ToString ()
                    |> sprintf "A workflow with id '%s' can't be found"
                    |> RequestErrors.UNPROCESSABLE_ENTITY
                | UndefinedVersion versionedWorkflowId ->
                    versionedWorkflowId.ToString ()
                    |> sprintf "A versioned, published workflow with id '%s' can't be found"
                    |> RequestErrors.UNPROCESSABLE_ENTITY
                | Invalid invalidWorkflowError ->
                    RequestErrors.UNPROCESSABLE_ENTITY invalidWorkflowError.Message
                | DuplicateStateName dupError ->
                    sprintf "Workflow '%s' already has a state named '%s'" (dupError.WorkflowId.ToString()) dupError.StateName
                    |> RequestErrors.UNPROCESSABLE_ENTITY
                | UndefinedState undefinedStateError ->
                    sprintf "Workflow '%s' does not have a state with id '%i'"
                        (undefinedStateError.WorkflowId.ToString())
                         undefinedStateError.UndefinedState
                    |> RequestErrors.UNPROCESSABLE_ENTITY
                | CantRemoveInitialState _ ->
                    RequestErrors.UNPROCESSABLE_ENTITY "Can't remove initial state"
                | InitialStateCantBeTerminalState _ ->
                    RequestErrors.UNPROCESSABLE_ENTITY "Initial state can't be set as a terminal state"
                | UndefinedTransition undefinedTransitionError ->
                    sprintf "Workflow '%s' does not have a transition with id '%i'"
                        (undefinedTransitionError.WorkflowId.ToString())
                         undefinedTransitionError.UndefinedTransition
                    |> RequestErrors.UNPROCESSABLE_ENTITY
                | DuplicateTransition dupTransition ->
                    sprintf "Workflow '%s' already has transition (id: '%i', name: '%s') from state '%i' to state '%i'"
                        (dupTransition.WorkflowId.ToString())
                         dupTransition.ErrorTransition.Id
                         dupTransition.ErrorTransition.Name
                         dupTransition.ErrorTransition.SourceState
                         dupTransition.ErrorTransition.TargetState
                    |> RequestErrors.UNPROCESSABLE_ENTITY
                | DuplicateTransitionName dupError ->
                    sprintf "Workflow '%s' already has a transition named '%s'" (dupError.WorkflowId.ToString()) dupError.ErrorTransition.Name
                    |> RequestErrors.UNPROCESSABLE_ENTITY
                | WorkflowError.Unknown (workflowId,ex) ->
                    sprintf "Error processing command for workflow with id '%s': %O" (workflowId.ToString ()) ex
                    |> ServerErrors.INTERNAL_ERROR)
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
    get "/workflow" (formatErrors getAllWorkflows)
    post "/workflow" (formatErrors sendCommand)
    postf "/workflow/%s" (checkGuid appendIdandSend >> formatErrors)
    getf "/workflow/%s/events" (checkGuid getEvents >> formatErrors)
    getf "/workflow/%s" (checkGuid getWorkflow >> formatErrors) }




let clientPath = System.IO.Path.Combine("../", "Client") |> System.IO.Path.GetFullPath

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

    Saturn.Application.run app
    0
