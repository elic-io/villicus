namespace  Villicus.CommandHandlers

open Villicus
open Villicus.Domain

type ReadStreamReply<'event,'error> = Result<'event list * int64 * int64 option,'error>

type ReadStream<'aggregateId,'event,'error> = {
    AggregateId: 'aggregateId
    FirstEventId: int64
    BufferSize: int
    ReplyChannel: AsyncReplyChannel<ReadStreamReply<'event,'error>> }
  with
    static member New<'event> (aggregateId:'aggregateId) firstEventId bufferSize (replyChannel:AsyncReplyChannel<ReadStreamReply<'event,'error>>) = {
        AggregateId = aggregateId
        FirstEventId = firstEventId
        BufferSize = bufferSize
        ReplyChannel = replyChannel }

type ResultCommand<'command,'state,'error> = {
    Command: 'command
    ReplyChannel: AsyncReplyChannel<Result<'state,'error>> }
  with
    static member New replyChannel command = {
        Command = command
        ReplyChannel = replyChannel }

type WFCommand<'command,'event,'aggregateId,'state,'error> =
| GetState of aggregateId:'aggregateId * version:System.Int64 * replyChannel:AsyncReplyChannel<Result<'state,'error>>
| GetVersionedState of versionedWorkflowId:VersionedWorkflowId * replyChannel:AsyncReplyChannel<Result<'state,'error>>
| ReadStream of ReadStream<'aggregateId,'event,'error>
| ResultCommand of ResultCommand<'command,'state,'error>

type VersionType =
| WorkflowVersion of Version
| EventId of int64

type Agent<'T> = MailboxProcessor<'T>

type Dispatcher<'Msg,'event> = {
    //TODO: would like to make this "inherit" MBP instead ...
    Agent: Agent<'Msg>
    Observable: System.IObservable<'event>
    CancellationTokenSource: System.Threading.CancellationTokenSource }

module Common =

    let sendEvents readStream startVersion streamOut aggregateIdStr =
        let rec stream version =
            async {
            let! events, _, nextEvent =
                readStream aggregateIdStr version 500
            events |> List.iter(Some >> streamOut >> Async.Start)
            match nextEvent with
            | None ->
                None |> streamOut |> Async.Start
                return ()
            | Some n ->
                return! stream n }
        stream startVersion


module Workflow =

    let getWorkflowId = function
        | GetState (w,_,_) -> w
        | GetVersionedState (vw,_) -> vw.Id
        | ReadStream rs -> rs.AggregateId
        | ResultCommand p -> Workflow.workflowId p.Command

    let streamIdString (workflowId:System.Guid) = sprintf "Workflow-%O" workflowId

    let load readStream maxVer takeUpTo aggregateIdStr =
        let rec fold state version = async {
            let! events, (lastEvent:int64), nextEvent =
                readStream aggregateIdStr version 500

            let state = events |> takeUpTo (int version) maxVer |> List.fold Workflow.evolve state
            match nextEvent with
            | None -> return lastEvent, state
            | Some n -> return! fold state n }
        fold None 0L

    let internal takeUpTo mn mx = 
        match mx with
        | EventId m ->
            let f = List.indexed >> List.takeWhile(fun (i,_) -> i < (int m) - mn) >> List.map snd
            match m with | 0L -> id | _ -> f
        | WorkflowVersion v ->
            let mapper (workVer:uint64) ev =
                let ver =
                    match ev with
                    | VersionIncremented e -> e.Version |> fun (Version x) -> x
                    | _ -> workVer
                let outEvent =
                    match ver > workVer with
                    | true -> None
                    | false -> Some ev
                (outEvent,ver)
            let f = (List.mapFold mapper 0UL) >> fst >> (List.choose id)
            match v with | (Version 0UL) -> id | _ -> f

    // this is the "repository"
    let internal save appendToStream workflowId (expectedVersion:int64) events =
        appendToStream (streamIdString workflowId) expectedVersion events
    
    let internal processReadStream (rdStrm:ReadStream<WorkflowId,WorkflowEvent,exn>) readStream =
        let idString = rdStrm.AggregateId |> fun (WorkflowId x) -> x |> streamIdString
        readStream idString rdStrm.FirstEventId rdStrm.BufferSize
        |> Async.map(Ok >> rdStrm.ReplyChannel.Reply) |> Async.Start

    let start (eventStore:Persistence.StreamDataStore<string,WorkflowEvent>) sendToObservers workflowId =
        Agent.Start<| fun inbox ->
            let rec loop (version,state) = async {
                match! inbox.Receive() with
                | GetState ((WorkflowId workflowId),ver,replyChannel) ->
                    match ver = version || ver = 0L with
                      | true -> Async.result (version,state)
                      | false -> load eventStore.ReadStream (EventId ver) takeUpTo (streamIdString workflowId)
                    |> Async.map(Ok >> replyChannel.Reply) |> Async.Start
                    return! loop (version,state)
                | GetVersionedState (versionedWorkflowId,replyChannel) ->
                    state |> Option.map(fun st -> 
                        match versionedWorkflowId.Version = st.Version with
                          | true -> Async.result (version,state)
                          | false ->
                            versionedWorkflowId.Id
                            |> (fun (WorkflowId x) -> streamIdString x)
                            |> load eventStore.ReadStream (WorkflowVersion versionedWorkflowId.Version) takeUpTo
                        |> Async.map(Ok >> replyChannel.Reply) |> Async.Start)
                    |> ignore
                    return! loop (version,state)
                | ReadStream (rdStrm) ->
                    processReadStream rdStrm eventStore.ReadStream
                    return! loop (version,state)
                | ResultCommand command ->
                    let eventResult = Workflow.handle command.Command state
                    match eventResult with
                    | Ok eList ->
                        let events = eList |> Seq.ofList
                        match! save eventStore.AppendToStream workflowId version events with
                            | Ok (_:unit) ->
                                let newState = Seq.fold Workflow.evolve state events
                                (version,newState) |> Ok |> command.ReplyChannel.Reply
                                events |> Seq.iter sendToObservers
                                return! loop ((version + (Seq.length events |> int64)), newState)
                            | Error e ->
                                e |> Error |> command.ReplyChannel.Reply
                                return! loop (version,state)
                    | Error e -> 
                        e |> Error |> command.ReplyChannel.Reply
                        return! loop (version,state)
                }
            load eventStore.ReadStream (EventId 0L) takeUpTo (streamIdString workflowId) |> Async.bind loop

    let createDispatcher observableBufferSize eventStore =
        let cts = new System.Threading.CancellationTokenSource ()
        let observable,broadcast = Observable.createObservableAgent<WorkflowEvent> observableBufferSize cts.Token
        let forward (agent: Agent<_>) command = agent.Post command

        let agent =
            Agent.Start
            <| fun inbox ->
                let rec loop aggregates = async {
                    let! command = inbox.Receive()
                    let id = getWorkflowId command
                    match Map.tryFind id aggregates with
                    | Some aggregate ->
                        forward aggregate command
                        return! loop aggregates
                    | None ->
                        let aggregate = 
                            match id with 
                            | WorkflowId w -> start eventStore broadcast w
                        forward aggregate command
                        return! loop (Map.add id aggregate aggregates) }
                loop Map.empty
        { Agent = agent
          Observable = observable
          CancellationTokenSource = cts }

    let versionProjection dispatcher save =
        dispatcher.Observable
        |> Observable.add(function
            | WorkflowPublished e -> Some (e,Published)
            | WorkflowWithdrawn e -> Some (e,Withdrawn)
            | _ -> None
            >> Option.map (fun (e,pubFun) ->
                dispatcher.Agent.PostAndReply(fun r -> GetVersionedState (e,r))
                |> Result.map(snd >> Option.map(pubFun >> save e >> Async.Ignore)))
            >> ignore)

module Journey =
    open Villicus.Persistence

    let streamIdString (journeyId:System.Guid) = sprintf "Journey-%O" journeyId

    let load<'t> readStream (evolve:Journey<'t> -> JourneyEvent<'t> -> Journey<'t>) maxVer aggregateIdStr =
        let takeUpTo mn mx =
            let f = List.indexed >> List.takeWhile(fun (i,_) -> i < (int mx) - mn) >> List.map snd
            match mx with | 0L -> id | _ -> f
        let rec fold state version = async {
            let! events, (lastEvent:int64), nextEvent =
                readStream aggregateIdStr version 500

            let state = events |> takeUpTo (int version) maxVer |> List.fold evolve state
            match nextEvent with
            | None -> return lastEvent, state
            | Some n -> return! fold state n }
        fold Journey<'t>.NewNonExisting 0L


    // this is the "repository"
    let inline internal save appendToStream workflowId (expectedVersion:int64) events =
        appendToStream (streamIdString workflowId) expectedVersion events
    
    let inline internal processReadStream (rdStrm:ReadStream<JourneyId,JourneyEvent<'a>,exn>) readStream =
        let idString = rdStrm.AggregateId |> fun (JourneyId x) -> x |> streamIdString
        readStream idString rdStrm.FirstEventId rdStrm.BufferSize
        |> Async.map(Ok >> rdStrm.ReplyChannel.Reply) |> Async.Start

    let start<'a> (eventStore:Persistence.StreamDataStore<string,JourneyEvent<'a>>) getWorkflow sendToObservers journeyId =
        let workflow = getWorkflow >> function | Published x | Withdrawn x -> x
        Agent.Start<| fun inbox ->
            let rec loop (version,state) = async {
                let evolve = workflow >> Journey.evolve<'a>
                let! (command:ResultCommand<JourneyCommand<'a>,int64*Journey<'a>,exn>) = inbox.Receive()
                let eventResult = Journey.handle<'a> (fun _ -> workflow () |> Ok) command.Command state
                match eventResult with
                | Ok eList ->
                    let events = eList |> Seq.ofList
                    match! save eventStore.AppendToStream journeyId version events with
                    | Ok _ ->
                        let newState = Seq.fold (evolve ()) state events
                        (version,newState) |> Ok |> command.ReplyChannel.Reply
                        events |> Seq.iter (sendToObservers)
                        return! loop ((version + (Seq.length events |> int64)), newState)
                    | Error e ->
                        e |> Error |> command.ReplyChannel.Reply
                        return! loop (version,state)
                | Error e -> 
                    e |> Error |> command.ReplyChannel.Reply
                    return! loop (version,state)
                }
            let evolve = workflow () |> Journey.evolve<'a>
            load eventStore.ReadStream evolve 0L (streamIdString journeyId)
            |> Async.bind loop
    
    let createDispatcher<'a> observableBufferSize eventStore (workflowRepo:Repository<VersionedWorkflowId,Published<WorkflowModel>>) =
        let cts = new System.Threading.CancellationTokenSource ()
        let observable,broadcast = Observable.createObservableAgent<JourneyEvent<'a>> observableBufferSize cts.Token
        let forward (agent: Agent<_>) command = agent.Post command

        let agent =
            Agent.Start
            <| fun inbox ->
                let rec loop aggregates = async {
                    let! command = inbox.Receive()
                    let id = Journey.journeyId command.Command
                    match Map.tryFind id aggregates with
                    | Some aggregate ->
                        forward aggregate command
                        return! loop aggregates
                    | None ->
                        match command.Command with
                        | CreateJourney c -> 
                            let guid = id |> fun (JourneyId j) -> j
                            match! workflowRepo.Retrieve c.VersionedWorkflowId with
                            | Some workflow ->
                                let aggregate = start<'a> eventStore (fun () -> workflow) broadcast guid
                                forward aggregate command
                                return! loop (Map.add id aggregate aggregates)
                            | None ->
                                "workflow not found" |> exn |> Error |> command.ReplyChannel.Reply
                                return! loop aggregates
                        | _ -> 
                            "workflow not found" |> exn |> Error |> command.ReplyChannel.Reply
                            return! loop aggregates }
                loop Map.empty
        { Agent = agent
          Observable = observable
          CancellationTokenSource = cts }
