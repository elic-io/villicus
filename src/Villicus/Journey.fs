namespace Villicus.Domain

type JourneyId = JourneyId of System.Guid

type JourneyEvent<'t> =
| JourneyCreated of JourneyCreationEvent<'t>
| Transitioned of TransitionEvent  
| Terminated of TerminationEvent
| ReActivated of JourneyId
and JourneyCreationEvent<'t> = { JourneyId: JourneyId; VersionedWorkflowId: VersionedWorkflowId; Subject: 't }
and TerminationEvent = { JourneyId: JourneyId; TerminalStateId: StateId; TerminalStateName: string }
and TransitionEvent = { JourneyId: JourneyId; TransitionId: TransitionId; State: State }

type JourneyCommand<'t> =
| CreateJourney of CreateJourneyCommand<'t>
| TransitionCommand of TransitionCommand
| ReActivate of TransitionCommand
and CreateJourneyCommand<'t> = { JourneyId: JourneyId; VersionedWorkflowId: VersionedWorkflowId; Subject: 't }
and TransitionCommand = { JourneyId: JourneyId; TransitionId: TransitionId }

type Journey<'t> = 
| ActiveJourney of JourneyModel<'t>
| TerminatedJourney of JourneyModel<'t>
| NonExistingJourney of VersionedWorkflowId
and JourneyModel<'t> = {
    JourneyId: JourneyId
    Workflow: VersionedWorkflowId
    Subject: 't
    CurrentState: State }
  with 
    member x.ActiveTransitions = x.CurrentState.To

type Journey<'t> with
    static member NewNonExisting : Journey<'t> = 
        { Id = WorkflowId System.Guid.Empty; Version = Version 0UL }
        |> NonExistingJourney

type Published<'a> =
        | Published of 'a
        | Withdrawn of 'a
    with
        member x.Workflow = match x with | Published x' | Withdrawn x' -> x'

type JourneyException (message, journeyId) = 
    inherit exn(sprintf "Error for %O: %s" journeyId message)
    member __.JourneyId = journeyId
    static member New i m = JourneyException(m,i)

type JourneyError =
    | UnknownJourneyError of JourneyId * exn
    | DuplicateJourneyId of JourneyId
    | NonExistant
    | NonExistantJourneyId of JourneyId
    | InvalidTransition of InvalidTransitionError
    | UndefinedTransition of UndefinedTransitionError
    | UndefinedVersion of VersionedWorkflowId
and InvalidTransitionError = {
    JourneyId: JourneyId
    TransitionId: TransitionId
    StateId: StateId }

module JourneyError =
    let unknown journeyId e = UnknownJourneyError (journeyId,e)
    let duplicateJourneyId = DuplicateJourneyId
    let nonExistantJourneyId = NonExistantJourneyId
    let invalidTransition journeyId transitionId stateId =
        { JourneyId = journeyId
          TransitionId = transitionId
          StateId = stateId }
        |> InvalidTransition
    let undefinedTransition workflowId transitionId =
        UndefinedTransition { WorkflowId = workflowId; UndefinedTransition = transitionId }
    let undefinedVersion = UndefinedVersion

module Journey =

    let journeyId = function
        | CreateJourney c -> c.JourneyId
        | TransitionCommand c -> c.JourneyId
        | ReActivate c -> c.JourneyId

    let versionedWorkflowId = function
        | ActiveJourney jm | TerminatedJourney jm -> jm.Workflow
        | NonExistingJourney i -> i
    
    let createJourney<'t> lookupWorkflow (command: CreateJourneyCommand<'t>) (state:Journey<'t>) =
        match state with
        | NonExistingJourney _ ->
            lookupWorkflow command.VersionedWorkflowId
            |> Result.bind(fun _ ->
                [ { JourneyCreationEvent.JourneyId = command.JourneyId
                    JourneyCreationEvent.VersionedWorkflowId = command.VersionedWorkflowId
                    JourneyCreationEvent.Subject = command.Subject } |> JourneyCreated ]
                |> Ok)
        | _ -> JourneyError.duplicateJourneyId command.JourneyId |> Error

    let toResult journeyId =
        function
        | NonExistingJourney _ -> journeyId |> JourneyError.nonExistantJourneyId |> Error
        | ActiveJourney a -> Ok a 
        | TerminatedJourney t -> Ok t

    let internal bindJourneyState journeyId (f) = toResult journeyId >> Result.bind f

    let transition<'t> (command: TransitionCommand) (workflow:WorkflowModel) : (Journey<'t> -> Result<JourneyEvent<'t> list,JourneyError>) =
        (fun (s:JourneyModel<'t>) ->
            Map.tryFind command.TransitionId workflow.Transitions
            |> Result.ofOption (JourneyError.undefinedTransition workflow.WorkflowId command.TransitionId)
            |> Result.bind(fun transition ->
                match s.CurrentState.Away.Contains command.TransitionId with
                  | false ->
                    JourneyError.invalidTransition command.JourneyId command.TransitionId s.CurrentState.Id |> Error
                  | true ->
                    let termState = Map.find transition.TargetState workflow.States
                    seq {
                        yield
                          { JourneyId = command.JourneyId
                            TransitionId = command.TransitionId
                            State = termState } |> Transitioned
                        if workflow.TerminalStates.Contains transition.TargetState then
                            yield
                              { JourneyId = command.JourneyId
                                TerminalStateId = transition.TargetState
                                TerminalStateName = termState.Name } |> Terminated
                        else
                            ()
                    } |> Seq.toList |> Ok ))
        |> bindJourneyState command.JourneyId

    let reActivate<'t> (command: TransitionCommand) workflow : (Journey<'t> -> Result<JourneyEvent<'t> list,JourneyError>) =
        fun (s:JourneyModel<'t>) ->
            match workflow.TerminalStates.Contains s.CurrentState.Id with
              | false ->
                JourneyError.invalidTransition command.JourneyId command.TransitionId s.CurrentState.Id |> Error
              | true ->
                transition command workflow (ActiveJourney s)
                |> Result.map(List.append [ ReActivated command.JourneyId ])
        |> bindJourneyState command.JourneyId

    let handle<'t> workflowLookup cmd journey =
        let bindLookup journeyId commandHandler journeyState =
            toResult journeyId journeyState
            |> Result.bind (fun jm -> workflowLookup jm.Workflow)
            |> Result.bind(fun workflow -> commandHandler workflow journeyState)
        function
        | CreateJourney command -> createJourney<'t> workflowLookup command
        | TransitionCommand command -> transition<'t> command |> bindLookup command.JourneyId
        | ReActivate command -> reActivate<'t> command |> bindLookup command.JourneyId
        |> (fun f -> f cmd journey |> (Result.map List.toSeq))

    let evolve<'t> (workflow:WorkflowModel) (state:Journey<'t>) (event:JourneyEvent<'t>) =
        let ev =
          function
          | NonExistingJourney _, JourneyCreated e ->
              { JourneyId = e.JourneyId
                Workflow = e.VersionedWorkflowId
                Subject = e.Subject
                CurrentState = workflow.InitialState } |> ActiveJourney
          // only active journeys can transition
          | ActiveJourney s, Transitioned e -> ActiveJourney { s with CurrentState = e.State }
          | ActiveJourney s, Terminated _ -> TerminatedJourney s
          | TerminatedJourney s, ReActivated _ -> ActiveJourney s

          // defaults for other invalid events is to not change anything
          // should be caught by command error handling and return errors there
          | NonExistingJourney s, _ -> NonExistingJourney s
          | ActiveJourney s, _ -> ActiveJourney s
          | TerminatedJourney s, _ -> TerminatedJourney s
        ev (state,event)


    type CommandProcessor<'t,'eventId> =
        Sentinam.CommandProcessor<Journey<'t>,JourneyId,JourneyCommand<'t>,JourneyEvent<'t>,'eventId,JourneyError> 

    type ReadStreamReply<'t,'eventId> = Sentinam.ReadStreamReply<JourneyEvent<'t>,'eventId,JourneyError>

    type ReadStream<'t,'eventId> = Sentinam.ReadStream<JourneyId,JourneyEvent<'t>,'eventId,JourneyError>

    type Envelope<'t,'eventId> = Sentinam.Envelope<JourneyCommand<'t>,JourneyEvent<'t>,'eventId,JourneyId,Journey<'t>,JourneyError>

    type ResultCommand<'t,'eventId> = Sentinam.ResultCommand<JourneyCommand<'t>,Journey<'t>,'eventId,JourneyError>
