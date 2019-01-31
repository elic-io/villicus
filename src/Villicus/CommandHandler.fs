namespace  Villicus.CommandHandlers

open Villicus
open Villicus.Domain

type ProcessCommand<'command,'state,'error> = {
    Command: 'command
    ErrorThunk: Result<'state,'error> -> Async<unit> }
  with
    static member New errorThunk command = {
        Command = command
        ErrorThunk = errorThunk }
        

type Agent<'T> = MailboxProcessor<'T>

module Process =

    let streamIdString (workflowId:System.Guid) = sprintf "Workflow-%O" workflowId

    let internal load readStream workflowId =
        let rec fold state version =
            async {
            let! events, (lastEvent:int64), nextEvent =
                readStream (streamIdString workflowId) version 500

            let state = List.fold Workflow.evolve state events
            match nextEvent with
            | None -> return lastEvent, state
            | Some n -> return! fold state n }
        fold None 0L

    // this is the "repository"
    let internal save appendToStream workflowId (expectedVersion:int64) events =
        appendToStream (streamIdString workflowId) expectedVersion events
    
    let start readStream appendToStream sendToObservers workflowId =
        Agent.Start<| fun inbox ->
            let rec loop version state =
                async {
                    let! command = inbox.Receive()
                    let eventResult = Workflow.handle command.Command state
                    match eventResult with
                    | Ok eList ->
                        let events = eList |> Seq.ofList
                        match! save appendToStream workflowId version events with
                            | Ok (_:unit) ->
                                events |> Seq.iter (Ok >> sendToObservers)
                                let newState = Seq.fold Workflow.evolve state events
                                do! newState |> Ok |> command.ErrorThunk
                                return! loop (version + (Seq.length events |> int64)) newState
                            | Error e ->
                                e |> Error |> sendToObservers
                                do! e |> Error |> command.ErrorThunk
                                return! loop version state
                    | Error e -> 
                        e |> Error |> sendToObservers
                        do! e |> Error |> command.ErrorThunk
                        return! loop version state }
            async {
                let! version, state = load readStream workflowId 
                return! loop version state }

    let createDispatcher readStream appendToStream observableBuffer observableToken =
        
        let observable,broadcast = Observable.createObservableAgent<WorkflowEvent> observableBuffer observableToken
        let forward (agent: Agent<_>) command = agent.Post command

        let agent =
            Agent.Start
            <| fun inbox ->
                let rec loop aggregates =
                    async {
                        let! command = inbox.Receive()
                        let id = Workflow.workflowId command.Command
                        match Map.tryFind id aggregates with
                        | Some aggregate ->
                            forward aggregate command
                            return! loop aggregates
                        | None ->
                            let aggregate = 
                                match id with 
                                | WorkflowId w -> start readStream appendToStream broadcast w
                            forward aggregate command
                            return! loop (Map.add id aggregate aggregates)
                    }
                loop Map.empty
        agent,observable