namespace  Villicus.CommandHandlers

open Villicus
open Villicus.Domain

type WFCommand<'command,'event,'state,'error> =
| GetState of workflowId:WorkflowId * version:System.Int64 * replyChannel:AsyncReplyChannel<Result<'state,'error>>
| ReadStream of ReadStream<'event,'error>
| ProcCommand of ProcessCommand<'command,'state,'error>
and ProcessCommand<'command,'state,'error> = {
    Command: 'command
    ReplyChannel: AsyncReplyChannel<Result<'state,'error>> }
  with
    static member New replyChannel command = {
        Command = command
        ReplyChannel = replyChannel }
and ReadStreamReply<'event,'error> = Result<'event list * int64 * int64 option,'error>
and ReadStream<'event,'error> = {
    WorkflowId: WorkflowId
    FirstEventId: int64
    BufferSize: int
    ReplyChannel: AsyncReplyChannel<ReadStreamReply<'event,'error>> }
  with
    member x.WorkflowGuid = x.WorkflowId |> fun (WorkflowId g) -> g
    static member New<'event,'error> workflowId firstEventId bufferSize (replyChannel:AsyncReplyChannel<ReadStreamReply<'event,'error>>) = {
        WorkflowId = workflowId
        FirstEventId = firstEventId
        BufferSize = bufferSize
        ReplyChannel = replyChannel }


type Agent<'T> = MailboxProcessor<'T>

module Process =

    let getWorkflowId = function
        | GetState (w,_,_) -> w
        | ReadStream rs -> rs.WorkflowId
        | ProcCommand p -> Workflow.workflowId p.Command

    let streamIdString (workflowId:System.Guid) = sprintf "Workflow-%O" workflowId

    let internal sendEvents readStream startVersion streamOut workflowId =
        let rec stream version =
            async {
            let! events, _, nextEvent =
                readStream (streamIdString workflowId) version 500
            events |> List.iter(Some >> streamOut >> Async.Start)
            match nextEvent with
            | None ->
                None |> streamOut |> Async.Start
                return ()
            | Some n ->
                return! stream n }
        stream startVersion

    let internal load readStream maxVer workflowId =
        let takeUpTo mn mx = 
            let f = List.indexed >> List.takeWhile(fun (i,_) -> i < mx - mn) >> List.map snd
            match mx with | 0 -> id | _ -> f
        let rec fold state version = async {
            let! events, (lastEvent:int64), nextEvent =
                readStream (streamIdString workflowId) version 500

            let state = events |> takeUpTo (int version) (int maxVer) |> List.fold Workflow.evolve state
            match nextEvent with
            | None -> return lastEvent, state
            | Some n -> return! fold state n }
        fold None 0L

    // this is the "repository"
    let internal save appendToStream workflowId (expectedVersion:int64) events =
        appendToStream (streamIdString workflowId) expectedVersion events
    
    let processReadStream (rdStrm:ReadStream<WorkflowEvent,exn>) readStream =
        readStream (streamIdString rdStrm.WorkflowGuid) rdStrm.FirstEventId rdStrm.BufferSize
        |> Async.map(Ok >> rdStrm.ReplyChannel.Reply) |> Async.Start

    let start readStream appendToStream sendToObservers workflowId =
        Agent.Start<| fun inbox ->
            let rec loop version state = async {
                match! inbox.Receive() with
                | GetState ((WorkflowId workflowId),ver,replyChannel) ->
                    match ver = version || ver = 0L with
                      | true -> Async.result (version,state)
                      | false -> load readStream ver workflowId
                    |> Async.map(Ok >> replyChannel.Reply) |> Async.Start
                    return! loop version state
                | ReadStream (rdStrm) ->
                    processReadStream rdStrm readStream
                    return! loop version state
                | ProcCommand command ->
                    let eventResult = Workflow.handle command.Command state
                    match eventResult with
                    | Ok eList ->
                        let events = eList |> Seq.ofList
                        match! save appendToStream workflowId version events with
                            | Ok (_:unit) ->
                                let newState = Seq.fold Workflow.evolve state events
                                (version,newState) |> Ok |> command.ReplyChannel.Reply
                                events |> Seq.iter (Ok >> sendToObservers)
                                return! loop (version + (Seq.length events |> int64)) newState
                            | Error e ->
                                e |> Error |> command.ReplyChannel.Reply
                                return! loop version state
                    | Error e -> 
                        e |> Error |> command.ReplyChannel.Reply
                        return! loop version state
                }
            async {
                let! version, state = load readStream 0L workflowId
                return! loop version state }

    let createDispatcher readStream appendToStream observableBuffer observableToken =
        
        let observable,broadcast = Observable.createObservableAgent<WorkflowEvent> observableBuffer observableToken
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
                            | WorkflowId w -> start readStream appendToStream broadcast w
                        forward aggregate command
                        return! loop (Map.add id aggregate aggregates) }
                loop Map.empty
        agent,observable