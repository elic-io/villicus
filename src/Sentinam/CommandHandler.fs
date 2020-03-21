namespace Sentinam.CommandHandlers

module private Async =
    let map f value = async {
      let! v = value
      return f v
    }

    let bind f xAsync = async {
      let! x = xAsync
      return! f x
    }

type ReadStreamReply<'event,'eventId,'error> = Result<'event seq * 'eventId * 'eventId option,'error>

type ReadStream<'aggregateId,'event,'eventId,'error> = {
    AggregateId: 'aggregateId
    FirstEventId: 'eventId
    BufferSize: int
    ReplyChannel: AsyncReplyChannel<ReadStreamReply<'event,'eventId,'error>> }
module ReadStream =
    let inline create
        (aggregateId:'aggregateId)
        firstEventId 
        bufferSize
        (replyChannel:AsyncReplyChannel<ReadStreamReply<'event,'eventId,'error>>) = {
            AggregateId = aggregateId
            FirstEventId = firstEventId
            BufferSize = bufferSize
            ReplyChannel = replyChannel }

type ResultCommand<'command,'aggregate,'error> = {
    Command: 'command
    ReplyChannel: AsyncReplyChannel<Result<'aggregate,'error>> }
module ResultCommand =
    let inline create command replyChannel = {
        Command = command
        ReplyChannel = replyChannel }

type ResultCommandStream<'command,'event,'eventId,'error> = {
    Command: 'command
    ReplyChannel: AsyncReplyChannel<Result<'eventId * 'event seq,'error>> }
module ResultCommandStream =
    let inline create command replyChannel = {
        ResultCommandStream.Command = command
        ResultCommandStream.ReplyChannel = replyChannel }

type GetState<'aggregateId,'eventId,'aggregate,'error> = 'aggregateId * 'eventId * AsyncReplyChannel<Result<'aggregate,'error>>
module GetState =
    let create aggregateId version replyChannel =
        GetState (aggregateId,version,replyChannel)

type Command<'command,'event,'eventId,'aggregateId,'aggregate,'error> =
| GetState of GetState<'aggregateId,'eventId,'aggregate,'error>
| ReadStream of ReadStream<'aggregateId,'event,'eventId,'error>
| ResultCommand of ResultCommand<'command,'aggregate,'error>
| ResultCommandStream of ResultCommandStream<'command,'event,'eventId,'error>

module Command =
    let inline newCmd command replyChannel = ResultCommand { Command = command; ReplyChannel = replyChannel }
    let inline newCommandStream command replyChannel = ResultCommandStream { Command = command; ReplyChannel = replyChannel }
    let inline newReadStream aggregateId firstEventId bufferSize replyChannel =
        { AggregateId = aggregateId
          FirstEventId = firstEventId
          BufferSize = bufferSize
          ReplyChannel = replyChannel }
        |> ReadStream

type Agent<'T> = MailboxProcessor<'T>

type Dispatcher<'Msg,'event> = {
    //TODO: would like to make this "inherit" MBP instead ?
    Agent: Agent<'Msg>
    Observable: System.IObservable<'event>
    CancellationTokenSource: System.Threading.CancellationTokenSource }

type Evolve<'aggregate,'event> = 'aggregate -> 'event -> 'aggregate
type Load<'aggregate,'aggregateId,'event,'eventId> = Evolve<'aggregate,'event> -> 'aggregateId -> Async<'eventId*'aggregate>
type Save<'aggregateId,'eventId,'event> = 'aggregateId -> 'eventId -> 'event seq -> Async<Result<unit,exn>>
type ExnToAggregateError<'aggregateError> = exn -> 'aggregateError
type HandleCommand<'command,'aggregate,'event,'aggregateError> = 'command -> 'aggregate -> Result<'event seq, 'aggregateError>
type IncrementVersion<'event,'eventId> = 'event seq -> 'eventId -> 'eventId
type EventAction<'event> = 'event -> unit
type CommandToId<'command,'aggregateId> = 'command -> 'aggregateId
type IsInitCommand<'command,'aggregateId,'aggregateError> = 'command -> Result<'aggregateId,'aggregateError>

type CommandProcessor<'aggregate,'aggregateId,'command,'event,'eventId,'aggregateError> = {
    Evolve : Evolve<'aggregate,'event>
    Load: Load<'aggregate,'aggregateId,'event,'eventId>
    Save: Save<'aggregateId,'eventId,'event>
    ExnToAggregateError: ExnToAggregateError<'aggregateError>
    HandleCommand: HandleCommand<'command,'aggregate,'event,'aggregateError>
    IncrementVersion: IncrementVersion<'event,'eventId>
    // EventAction: EventAction<'event>
    // CommandToId: CommandToId<'command,'aggregateId>
    // IsInitCommand: IsInitCommand<'command,'aggregateId,'aggregateError>
}


module Common =

    let sendEvents readStream startVersion streamOut aggregateIdStr =
        let rec stream version =
            async {
            let! events, _, nextEvent =
                readStream aggregateIdStr version 500
            events |> Seq.iter(Some >> streamOut >> Async.Start)
            match nextEvent with
            | None ->
                None |> streamOut |> Async.Start
                return ()
            | Some n ->
                return! stream n }
        stream startVersion

    let getState aggregateId version replyChannel =
        GetState (aggregateId,version,replyChannel)

module Attorney =
    let load<'aggregate,'event,'eventId when 'eventId : comparison>
        (uninitializedAggregate:'aggregate)
        (minEventId:'eventId)
        (subtractEventId: 'eventId -> 'eventId -> 'eventId)
        (indexSeq: seq<'event> -> seq<'eventId*'event>)
        (readStream: 'eventId -> int -> Async<'event list * 'eventId * 'eventId option>)
        (evolve:'aggregate -> 'event -> 'aggregate)
        maxVer =
            let takeUpTo (mn:'eventId) (mx:'eventId) =
                let f = indexSeq >> Seq.takeWhile(fun (i,_) -> i < subtractEventId mx mn) >> Seq.map snd
                if mx = minEventId then id else f
            let rec fold state version = async {
                let! events, (lastEvent:'eventId), nextEvent =
                    readStream version 500
                let state = events |> takeUpTo version maxVer |> Seq.fold evolve state
                match nextEvent with
                | None -> return lastEvent, state
                | Some n -> return! fold state n }
            fold uninitializedAggregate minEventId


    // this is the "repository"
    let inline internal save appendToStream aggregateId (expectedVersion:'eventId) events =
        appendToStream aggregateId expectedVersion events
    
    let inline internal processReadStream (rdStrm:ReadStream<'aggregateId,'aggregateEvent,'eventId,'aggregateError>) readStream =
        readStream rdStrm.AggregateId rdStrm.FirstEventId rdStrm.BufferSize
        |> Async.map(Ok >> rdStrm.ReplyChannel.Reply) |> Async.Start

    let createPump<'aggregate,'aggregateId,'command,'event,'eventId,'aggregateError>
        (cp: CommandProcessor<'aggregate,'aggregateId,'command,'event,'eventId,'aggregateError>)
        (sendToObservers: EventAction<'event>)
        (cancellationToken: System.Threading.CancellationToken)
        (aggregateId:'aggregateId)
        =
            Agent.Start ((fun inbox ->
                let rec loop (version:'eventId,state:'aggregate) = async {
                    let! (command:ResultCommand<'command,'eventId*'aggregate,'aggregateError>) = inbox.Receive()
                    let eventResult = cp.HandleCommand command.Command state
                    match eventResult with
                    | Ok eList ->
                        match! eList |> cp.Save aggregateId version with
                        | Ok _ ->
                            let newState = Seq.fold cp.Evolve state eList
                            (version,newState) |> Ok |> command.ReplyChannel.Reply
                            eList |> Seq.iter sendToObservers
                            return! loop (cp.IncrementVersion eList version, newState)
                        | Error e ->
                            e |> cp.ExnToAggregateError |> Error |> command.ReplyChannel.Reply
                            return! loop (version,state)
                    | Error e -> 
                        e |> Error |> command.ReplyChannel.Reply
                        return! loop (version,state)
                    }
                cp.Load cp.Evolve aggregateId
                |> Async.bind loop)
              , cancellationToken)
    
    let createDispatcherAgent<'aggregate,'aggregateId,'aggregateEvent,'eventId,'command,'aggregateError
            when 'aggregateId : comparison>
        (commandToId: CommandToId<'command,'aggregateId>)
        (isInitCommand: IsInitCommand<'command,'aggregateId,'aggregateError>)
        observableBufferSize
        (startPump: ('aggregateEvent -> unit) -> System.Threading.CancellationToken -> 'aggregateId -> Agent<ResultCommand<'command,'eventId * 'aggregate,'aggregateError>>) =
            let cts = new System.Threading.CancellationTokenSource ()
            let (observable,broadcast) = Sentinam.Observable.createObservableAgent<'aggregateEvent> observableBufferSize cts.Token
            let forward (agent: Agent<_>) command = agent.Post command
            let agent =
                Agent.Start ((fun inbox ->
                    let rec loop aggregates = async {
                        let! (command:ResultCommand<'command,'eventId*'aggregate,'aggregateError>) = inbox.Receive()
                        let iD = commandToId command.Command
                        match Map.tryFind iD aggregates with
                        | Some aggregate ->
                            forward aggregate command
                            return! loop aggregates
                        | None ->
                            match isInitCommand command.Command with
                            | Ok iD ->
                                let aggregate = startPump broadcast cts.Token iD
                                forward aggregate command
                                return! loop (Map.add iD aggregate aggregates)
                            | Error e -> 
                                e |> Error |> command.ReplyChannel.Reply
                                return! loop aggregates
                    }
                    loop Map.empty)
                  , cts.Token)
            { Agent = agent
              Observable = observable
              CancellationTokenSource = cts }
