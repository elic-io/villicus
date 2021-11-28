namespace  Villicus.CommandHandlers

open Villicus
open Villicus.Domain
open Sentinam

type VersionType =
| WorkflowVersion of Version
| EventId of int64

module private Seq =

    // not near as versatile or performant as Seq.indexed, but it should work fine for our purposes
    let indexedU64 (xs:'t seq) =
        //todo: should this mutable be boxed in a "cell" ? singlethreaded, so doubtful, but check anyway
        let mutable i = 0UL
        xs
        |> Seq.map (fun x ->
            let x' = (i,x)
            i <- i + 1UL
            x')

module Workflow =

    open Sentinam

    
    let start (eventStore:Persistence.StreamDataStore<string,WorkflowEvent,uint64>) =
        let load readStream evolve maxVer aggregateId =
            load None 0UL (-) Seq.indexedU64 (readStream aggregateId) evolve maxVer
        let streamIdString (WorkflowId workflowId) = sprintf "Workflow-%O" workflowId
        let readStream batchSize workflowId maxEvent =
            let workflowIdString = streamIdString workflowId
            eventStore.ReadStream batchSize workflowIdString maxEvent 

        {   Evolve = Workflow.evolve
            Load = load (streamIdString >> (eventStore.ReadStream 500))
            Save = streamIdString >> eventStore.AppendToStream
            ExnToAggregateError = WorkflowError.unknown
            HandleCommand = fun c -> Workflow.handle c >> Result.map List.toSeq
            IncrementVersion = (fun xs i -> Seq.length xs |> uint64 |> (+) i)
            MinimumEventId = 0UL
            ReadStream = readStream }
        |> createAgent

    let storeAggregate
        (c:Workflow.Envelope<uint64>)
        (aggs: Map<WorkflowId,Agent<Workflow.Envelope<uint64>>>)
        (iD: WorkflowId)
        (startPump: unit -> Agent<Workflow.Envelope<uint64>>)
        = async {
            let aggregateAgent = startPump ()
            aggregateAgent.Post c
            return (Map.add iD aggregateAgent aggs)
        }



    let createDispatcherAgent
        (broadcast:EventAction<WorkflowEvent>)
        (cancellationToken:System.Threading.CancellationToken)
        (eventStore:Persistence.StreamDataStore<string,WorkflowEvent,uint64>) =
            createDispatcherAgent
                Workflow.workflowId
                cancellationToken
                storeAggregate
                (start eventStore broadcast)

    let versionProjection (agent:Agent<Workflow.Envelope<uint64>>) save =
        function
        | WorkflowPublished e -> Some (e,Published)
        | WorkflowWithdrawn e -> Some (e,Withdrawn)
        | _ -> None
        >> Option.map (fun (e,pubFun) ->
            let (workflowId,version) = VersionedWorkflowId.Decompose e
            agent.PostAndReply(fun r -> GetState (workflowId,version,r))
            |> Result.map(snd >> Option.map(pubFun >> save e >> Async.Ignore)))
        >> ignore

module Journey =

    open Sentinam

    let load<'t> readStream evolve maxVer aggregateId =
        load<Journey<'t>,JourneyEvent<'t>,uint64> Journey<'t>.NewNonExisting 0UL (-) Seq.indexedU64 (readStream aggregateId) evolve maxVer
    
    let start<'t>
        (getWorkflow: unit -> Published<WorkflowModel>)
        (eventStore:Persistence.StreamDataStore<string,JourneyEvent<'t>,uint64>)
        =
            let getWorkflow = getWorkflow >> function | Published x | Withdrawn x -> x
            let streamIdString (JourneyId journeyId) = sprintf "Journey-%O" journeyId
            let readStream batchSize journeyId maxEvent =
                let journeyIdString = streamIdString journeyId
                eventStore.ReadStream batchSize journeyIdString maxEvent 

            {   Evolve = getWorkflow () |> Journey.evolve<'t>
                Load = load<'t> (streamIdString >> (eventStore.ReadStream 500))
                Save = streamIdString >> eventStore.AppendToStream
                ExnToAggregateError = JourneyError.unknown
                HandleCommand = Journey.handle<'t> (fun _ -> getWorkflow () |> Ok)
                IncrementVersion = (fun xs i -> Seq.length xs |> uint64 |> (+) i)
                MinimumEventId = 0UL
                ReadStream = readStream }
            |> createAgent

    let createDispatcherAgent<'a>
        eventStore
        (cancellationToken: System.Threading.CancellationToken)
        (broadcast: JourneyEvent<'a> -> unit)
        (workflowRepo:Persistence.Repository<VersionedWorkflowId,Published<WorkflowModel>>)
        =
            let forward (agent: Agent<_>) command = agent.Post command
            Agent.Start <| fun inbox ->
                let rec loop aggregates = async {
                    let! (command:Journey.Envelope<'a,uint64>) = inbox.Receive()
                    let iD = command |> (getAggregateId Journey.journeyId)
                    match Map.tryFind iD aggregates with
                    | Some aggregate ->
                        forward aggregate command
                        return! loop aggregates
                    | None ->
                        match command with
                        | ResultCommand c' ->
                            match c'.Command with
                            | CreateJourney c'' ->
                                match! workflowRepo.Retrieve c''.VersionedWorkflowId with
                                | Some workflow ->
                                    let aggregate = start<'a> (fun () -> workflow) eventStore broadcast cancellationToken iD
                                    forward aggregate command
                                    return! loop (Map.add iD aggregate aggregates)
                                | None ->
                                    JourneyError.undefinedVersion c''.VersionedWorkflowId |> Error |> c'.ReplyChannel.Reply
                                    return! loop aggregates
                            | _  ->
                                c'.Command |> Journey.journeyId |> JourneyError.nonExistantJourneyId |> Error |> c'.ReplyChannel.Reply
                                return! loop aggregates
                        | GetState (journeyId,_,replyChannel)  ->
                            journeyId |> JourneyError.nonExistantJourneyId |> Error |> replyChannel.Reply
                            return! loop aggregates
                        | ReadStream c' ->
                            c'.AggregateId |> JourneyError.nonExistantJourneyId |> Error |> c'.ReplyChannel.Reply
                            return! loop aggregates
                        | ResultCommandStream c' ->
                            match c'.Command with
                            | CreateJourney c'' ->
                                match! workflowRepo.Retrieve c''.VersionedWorkflowId with
                                | Some workflow ->
                                    let aggregate = start<'a> (fun () -> workflow) eventStore broadcast cancellationToken iD
                                    forward aggregate command
                                    return! loop (Map.add iD aggregate aggregates)
                                | None ->
                                    JourneyError.undefinedVersion c''.VersionedWorkflowId |> Error |> c'.ReplyChannel.Reply
                                    return! loop aggregates
                            | _  ->
                                c'.Command |> Journey.journeyId |> JourneyError.nonExistantJourneyId |> Error |> c'.ReplyChannel.Reply
                                return! loop aggregates }
                loop Map.empty


