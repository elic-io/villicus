open Sentinam.Persistence.Memory
open Villicus
open Villicus.Domain
open Villicus.Serialization
#if FABLE_COMPILER
open Thoth.Json
#else
open Thoth.Json.Net
#endif

type APIError =
    | Unknown of exn
    | NotFound of System.Guid
    | BadRequest of string
    | InvalidUUID of string
    | CommandCreation of CommandCreationError
    | WorkflowError of WorkflowError



let storeNames save =
    function
    | WorkflowRenamed e -> Some (e.WorkflowId,e.Name)
    | WorkflowCreated e -> Some (e.WorkflowId,e.Name)
    | _ -> None
    >> Option.iter (fun (a,b) -> save a b |> ignore)

let repo = MemoryRepository.create<VersionedWorkflowId,Published<WorkflowModel>> ()
let workFlowByNameRepo = MemoryRepository.create<WorkflowId,string> ()


let cts = new System.Threading.CancellationTokenSource ()

let wfDispatcher =
    let memStore = MemoryEventStore.create<string,WorkflowEvent,uint64> ()
    let observable,broadcast = Observable.createObservableAgent<WorkflowEvent> 500 cts.Token

    let dispatcher =
        CommandHandlers.Workflow.createDispatcherAgent broadcast cts.Token memStore
    observable.Add (CommandHandlers.Workflow.versionProjection dispatcher repo.Save)
    //CommandHandlers2.Workflow.versionProjection dispatcher repo.Save
    Observable.add (storeNames workFlowByNameRepo.Save) observable
    dispatcher

let jDispatcher =
    let es = MemoryEventStore.create<string,JourneyEvent<System.Guid>,uint64> ()
    CommandHandlers.Journey.createDispatcherAgent es cts.Token (fun _ -> ())  repo


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

let getIntOption (name:string) (minVal:uint64) (maxVal:uint64) (defaultVal:uint64) options =
    match options |> Map.tryFind (name.ToLower()) with
    //todo: validate pagesize ?
    | Some (s:string) -> match System.UInt64.TryParse(s) with | true,n -> n | _ -> defaultVal
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
    let priorId = System.Math.Max(0UL,eventIndex - (uint64 pageSize))
    let prev = linkheader priorId pageSize "prev"
    let first = linkheader 0UL pageSize "first"
    let next nextId = linkheader nextId pageSize "next"
    match eventIndex,nextEventId with
    | (0UL,None) -> first
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
        Sentinam.Envelope.newCommandStream
        >> wfDispatcher.PostAndAsyncReply
        >> Async.StartAsTask
        >> Task.map(
          Result.map2
            (fun (eventIndex,eventList) ->
                (eventList
                |> Seq.map (WorkflowEvent.Encoder)
                |> Encode.seq
                |> Encode.toString 4
                |> Successful.CREATED)     
                >=> (eventIndex |> string |> setHttpHeader "Etag"))
            WorkflowError))
    >> function
    | Ok x -> x
    | Error e -> e |> APIError.CommandCreation |> Error |> Task.FromResult

let postText (ctx:HttpContext) =
    use body = new System.IO.StreamReader(ctx.Request.Body)
    //The modelbinder has already read the stream and need to reset the stream index ???
    // body.BaseStream.Seek(0L, System.IO.SeekOrigin.Begin) |> ignore
    body.ReadToEnd ()

let sendCommand : HttpContext -> Task<Result<HttpHandler,APIError>> = fun (ctx:HttpContext) ->
    ctx |> postText |> Decode.fromString WorkflowCommand.Decoder
    |> function
        | Ok x -> processCommand x
        | Error e -> e |> BadRequest |> Error |> Task.FromResult

let appendIdandSend (wfid:System.Guid) = fun (ctx:HttpContext) ->
    let o =
        ctx |> postText |> Newtonsoft.Json.Linq.JObject.Parse :> Newtonsoft.Json.Linq.JToken
    o.["workflowId"] <- Newtonsoft.Json.Linq.JValue(wfid.ToString())
    WorkflowCommand.Decoder "" o
    |> function
        | Ok x -> processCommand x
        | Error err -> err.ToString() |> BadRequest |> Error |> Task.FromResult

let getEvents wfid : (HttpContext -> Task<Result<HttpHandler,APIError>>) = fun (ctx:HttpContext) ->
    let options = getOptions ctx
    let pageSize = 
        let pgSize = getIntOption "pagesize" 10UL 100UL 50UL options
        System.Convert.ToInt32(pgSize) // don't need to catch exn because we know it is in range 10-50
    let offSet = options |> getIntOption "offset" 0UL System.UInt64.MaxValue 0UL
    let postMsg = Sentinam.Envelope.newReadStream (WorkflowId wfid) offSet pageSize
    wfDispatcher.PostAndAsyncReply postMsg
    |> Async.StartAsTask
    |> Task.map(
        Result.mapError WorkflowError
        >> Result.bind(fun (eventList,eventIndex,nextEventId) ->
            match ((Seq.isEmpty eventList),offSet) with
            | true,0UL -> wfid |> NotFound |> Error
            | _ ->
              Ok(
                eventList
                |> Seq.map WorkflowEvent.Encoder
                |> Encode.seq
                |> Encode.toString 4
                |> Successful.OK
                >=> pagingHeaders pageSize eventIndex nextEventId)))


let getAllWorkflows _ =
    try
        (fun (_:HttpFunc) (ctx:HttpContext) ->
            use memStream = new System.IO.MemoryStream ()
            use itemStream = new System.IO.StreamWriter (memStream)
            let writeItem (wfid,name) =
                { Api.ViewTypes.WorkflowMetaListItem.WorkflowId = wfid; Api.ViewTypes.WorkflowMetaListItem.Name = name }
                |> Api.ViewTypes.WorkflowMetaListItem.Encoder
                |> Encode.toString 4
                |> itemStream.Write
            itemStream.Write "[/n"
            use observer =
                workFlowByNameRepo.RetrieveItems (fun _ _ -> true)
                |> FSharp.Control.Reactive.Observable.subscribeWithCompletion writeItem (fun () -> itemStream.Write "\n]")
            ctx.WriteStreamAsync
                    false // enableRangeProcessing
                    memStream
                    None // eTag
                    None // lastModified
        ) |> Ok
    with e -> e |> Unknown |> Error
    |> Task.FromResult


let getWorkflow wfid = fun (_:HttpContext) ->
    Sentinam.Envelope.newGetState (WorkflowId wfid) 0UL
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
    let ccErrorToResponse ccError =
        match ccError with
        | NullArgument errMsg -> errMsg
        | CantTargetSelf errMsg -> errMsg
        |> RequestErrors.BAD_REQUEST

    ctx |> f |> Task.bind(
        Result.mapError(function
            | Unknown e -> ServerErrors.INTERNAL_ERROR e.Message
            | NotFound guid -> guid.ToString() |> sprintf "WorkflowId '%s' not found" |> RequestErrors.NOT_FOUND
            | BadRequest e -> RequestErrors.BAD_REQUEST e
            | InvalidUUID guidStr -> guidStr |> sprintf "'%s' is not a valid UUID" |> RequestErrors.BAD_REQUEST
            | CommandCreation ccError ->
                ccErrorToResponse ccError
            | WorkflowError wfe ->
                match wfe with
                | WorkflowError.CommandCreation ccError ->
                    ccErrorToResponse ccError
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
