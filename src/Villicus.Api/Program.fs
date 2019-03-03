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

let clientPath = System.IO.Path.Combine("../", "Client") |> System.IO.Path.GetFullPath

let createFileRepo dataDir : Persistence.Repository<WorkflowId,WorkflowModel> =
    let readFromfile (file:string) =
        match System.IO.File.Exists file with
        | false -> Async.result None
        | true ->
            use sr = new System.IO.StreamReader(file)
            sr.ReadToEndAsync() |> Async.AwaitTask
            |> Async.Catch |> Async.map Result.ofChoice
            |> Async.map(Result.bind (Decode.fromString WorkflowModel.Decoder >> Result.mapError exn)
                >> Result.fold Some (fun _ -> None))
    let getFileName (iD:WorkflowId) = dataDir + iD.ToString()
    { Retrieve = getFileName >> readFromfile
      RetrieveAll = fun () ->
        System.IO.Directory.GetFiles(dataDir)
        |> Seq.toArray |> Array.map readFromfile
        |> Async.Parallel
        |> Async.map (Array.choose id >> Seq.ofArray)
      Save = (fun key value ->
        System.IO.File.WriteAllTextAsync(getFileName key, value |> WorkflowModel.Encoder |> Encode.toString 0)
        |> Async.AwaitTask
        |> Async.Catch |> Async.map Result.ofChoice) }

let inline runAndDiscard x = x |> Async.Ignore |> Async.Start
let projectToDir dispatcher (repo:Persistence.Repository<WorkflowId,WorkflowModel>) =
    let projector = // (datastore:Persistence.Repository<WorkflowId,WorkflowModel>) =
        MailboxProcessor.Start<| fun inbox ->
            let rec loop () = async {
                let! event = inbox.Receive ()
                do!
                  let workflowId = Workflow.eWorkflowId event
                  workflowId
                  |> repo.Retrieve
                  |> Async.bind(fun state -> 
                      event |> Workflow.evolve state |> Async.result |> Async.bind(
                        function 
                        | Some wf -> repo.Save workflowId wf |> Async.Ignore 
                        | None -> Async.result ()))
                return! loop () }
            loop ()            
    dispatcher.Observable |> Observable.add projector.Post


let getTempFilePath () =
    let path = System.IO.Path.Combine(System.IO.Path.GetTempPath() + "/", "workflowCache/")
    try
        System.IO.Directory.Delete path
    with _ -> ()
    System.IO.Directory.CreateDirectory path
    |> fun di -> di.FullName

[<EntryPoint>]
let main argv =

    let repo = Persistence.MemoryRepository.create<VersionedWorkflowId,Published<WorkflowModel>> ()

    let fileCacheDir = getTempFilePath ()
    printfn "file cache created at: '%s'" fileCacheDir
    
    let fileRepo =
      // createFileRepo fileCacheDir
      Persistence.MemoryRepository.create<WorkflowId,WorkflowModel> ()
    
    let wfDispatcher = 
        let d = Persistence.MemoryEventStore.create<string,WorkflowEvent> () |> Workflow.createDispatcher 0
        Workflow.versionProjection d repo.Save
        projectToDir d fileRepo
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
           | Error msg ->
                    (errorContent >=> RequestErrors.BAD_REQUEST msg) ctx
           | Ok wfCommand -> processCommand wfCommand ctx

    let appendIdandSend (wfidStr:string) : WebPart =
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

    let getAllWorkflows : WebPart =
        // fileRepo.RetrieveAll ()
        // |> Async.bind(Seq.toList 
        //              >> List.map WorkflowModel.Encoder >> Encode.list 
        //              >> Encode.toString 4 >> Successful.OK)
        fun (ctx:HttpContext) ->
            fileRepo.RetrieveAll ()
            |> Async.bind(fun all ->
                (all |> Seq.toList 
                 |> List.map WorkflowModel.Encoder |> Encode.list 
                 |> Encode.toString 4 |> Successful.OK) ctx)

    let getOptions ctx =
        ctx.request.query
        |> List.choose (fun (k,v) -> v |> Option.map (fun v' -> k.ToLower(),v')) |> Map.ofList

    let getIntOption (name:string) (minVal:int64) (maxVal:int64) (defaultVal:int64) options =
        match options |> Map.tryFind (name.ToLower()) with
        //todo: validate pagesize ?
        | Some (s:string) -> match System.Int64.TryParse(s) with | true,n -> n | _ -> defaultVal
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
                    |> fun (x:int64) -> System.Convert.ToInt32(x) // don't need to catch exn because we know it is in range 10-50
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
            [ path "/" >=> Files.file "index.html"
              path "" >=> Files.file "index.html"
              path "/api/workflow" >=> getAllWorkflows
              Filters.pathScan "/api/workflow/%s/events" (fun w -> getEvents w) 
              Filters.pathScan "/api/workflow/%s" getWorkflow
              RequestErrors.NOT_FOUND "Invalid Uri Path" ]
          POST >=> choose
            [ path "/api/workflow" >=> sendCommand
              Filters.pathScan "/api/workflow/%s" appendIdandSend
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

    try
      startWebServer startConfig app
    finally
      System.IO.Directory.Delete (fileCacheDir, true)
    0
