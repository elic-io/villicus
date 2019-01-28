namespace FlowMaster.Domain

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
| CreateJourney of CreateCommand<'t>
| Transition of TransitionCommand
| ReActivate of TransitionCommand
and CreateCommand<'t> = { JourneyId: JourneyId; VersionedWorkflowId: VersionedWorkflowId; Subject: 't }
and TransitionCommand = { JourneyId: JourneyId; TransitionId: TransitionId }

type Journey<'t> = 
| ActiveJourney of JourneyModel<'t>
| TerminatedJourney of JourneyModel<'t>
| NonExistingJourney
and JourneyModel<'t> = {
    JourneyId: JourneyId
    Workflow: VersionedWorkflowId
    Subject: 't
    CurrentState: State }
  with 
    member x.ActiveTransitions = x.CurrentState.To

<<<<<<< 4869de5db97fa87bb493d72531b59a7b28f0c649
<<<<<<< f406dde39e7ba907a7fe269125bf6103ba2a9b95
type JourneyException (message, journeyId) = 
=======
type JourneyException (message:string, journeyId:JourneyId) = 
>>>>>>> refactor module and namespaces to move types up into namespace and out of modules
=======
type JourneyException (message, journeyId) = 
>>>>>>> cleanup type inference on function parameters
    inherit exn(sprintf "Error for %O: %s" journeyId message)
    member __.JourneyId = journeyId
    static member New i m = JourneyException(m,i)

type DuplicateJourneyIdException (journeyId) = 
    inherit JourneyException("already exists",journeyId)

type NonExistantJourneyException (journeyId) =
    inherit JourneyException("journey does not exist", journeyId)
<<<<<<< f406dde39e7ba907a7fe269125bf6103ba2a9b95

type InvalidTransitionException (message,journeyId,transitionId,stateId) =
    inherit JourneyException(
        seq { 
            yield sprintf "%O is not valid from %O" transitionId stateId
            if System.String.IsNullOrWhiteSpace message then () else
                yield message }
            |> String.concat ": "
        , journeyId)
    with
    member __.TransitionId = transitionId
    member __.StateId = stateId
    new(journeyId,transitionId,stateId) =
        InvalidTransitionException ("",journeyId,transitionId,stateId)
=======
>>>>>>> refactor module and namespaces to move types up into namespace and out of modules

type InvalidTransitionException (message,journeyId,transitionId,stateId) =
    inherit JourneyException(
        seq { 
            yield sprintf "%O is not valid from %O" transitionId stateId
            if System.String.IsNullOrWhiteSpace message then () else
                yield message }
            |> String.concat ": "
        , journeyId)
    with
    member __.TransitionId = transitionId
    member __.StateId = stateId
    new(journeyId,transitionId,stateId) =
        InvalidTransitionException ("",journeyId,transitionId,stateId)

module Journey =

    let journeyId = function
        | CreateJourney c -> c.JourneyId
        | Transition c -> c.JourneyId
        | ReActivate c -> c.JourneyId

<<<<<<< 4869de5db97fa87bb493d72531b59a7b28f0c649
<<<<<<< f406dde39e7ba907a7fe269125bf6103ba2a9b95
    let createJourney lookupWorkflow (command: CreateCommand<'t>) =
=======
    let createJourney (lookupWorkflow:VersionedWorkflowId -> Result<WorkflowModel,exn>) (command: CreateCommand<'t>) =
>>>>>>> refactor module and namespaces to move types up into namespace and out of modules
=======
    let createJourney lookupWorkflow (command: CreateCommand<'t>) =
>>>>>>> cleanup type inference on function parameters
        function
        | NonExistingJourney ->
            lookupWorkflow command.VersionedWorkflowId
            |> Result.bind(fun _ ->
                [ { JourneyCreationEvent.JourneyId = command.JourneyId
                    JourneyCreationEvent.VersionedWorkflowId = command.VersionedWorkflowId
                    JourneyCreationEvent.Subject = command.Subject } |> JourneyCreated ]
                |> Ok)
        | _ -> DuplicateJourneyIdException(command.JourneyId) :> exn |> Error

    let toResult journeyId =
        function
        | NonExistingJourney -> journeyId |> NonExistantJourneyException :> exn |> Error
        | ActiveJourney a -> Ok a 
        | TerminatedJourney t -> Ok t

    let internal bindJourneyState journeyId f = toResult journeyId >> Result.bind f

<<<<<<< 4869de5db97fa87bb493d72531b59a7b28f0c649
<<<<<<< f406dde39e7ba907a7fe269125bf6103ba2a9b95
=======
    type InvalidTransitionException (message,journeyId,transitionId,stateId) =
        inherit JourneyException(
            seq { 
                yield sprintf "%O is not valid from %O" transitionId stateId
                if System.String.IsNullOrWhiteSpace message then () else
                    yield message }
              |> String.concat ": "
            , journeyId)
      with
        member __.TransitionId = transitionId
        member __.StateId = stateId
        new(journeyId,transitionId,stateId) = InvalidTransitionException ("",journeyId,transitionId,stateId)

>>>>>>> refactor module and namespaces to move types up into namespace and out of modules
=======
>>>>>>> cleanup type inference on function parameters
    let transition<'t> (command: TransitionCommand) (workflow:WorkflowModel) =
        (fun (s:JourneyModel<'t>) ->
            Map.tryFind command.TransitionId workflow.Transitions
            |> Result.ofOption (UndefinedTransitionException(workflow.WorkflowId,command.TransitionId) :> exn)
            |> Result.bind(fun transition ->
                match s.CurrentState.Away.Contains command.TransitionId with
                  | false ->
                    InvalidTransitionException(command.JourneyId,command.TransitionId,s.CurrentState.Id) :> exn |> Error
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

<<<<<<< 4869de5db97fa87bb493d72531b59a7b28f0c649
<<<<<<< f406dde39e7ba907a7fe269125bf6103ba2a9b95
    let reActivate<'t> (command: TransitionCommand) workflow =
=======
    let reActivate<'t> (command: TransitionCommand) (workflow:WorkflowModel) =
>>>>>>> refactor module and namespaces to move types up into namespace and out of modules
=======
    let reActivate<'t> (command: TransitionCommand) workflow =
>>>>>>> cleanup type inference on function parameters
        fun (s:JourneyModel<'t>) ->
            match workflow.TerminalStates.Contains s.CurrentState.Id with
              | false ->
                InvalidTransitionException(sprintf "current %O is not Terminal" s.CurrentState.Id,command.JourneyId,command.TransitionId,s.CurrentState.Id) :> exn |> Error
              | true ->
                transition command workflow (ActiveJourney s)
                |> Result.map(List.append [ ReActivated command.JourneyId ])
        |> bindJourneyState command.JourneyId

    let handle<'t> workflowLookup =
        let bindLookup journeyId commandHandler journeyState =
            toResult journeyId journeyState
            |> Result.bind (fun jm -> workflowLookup jm.Workflow)
            |> Result.bind(fun workflow -> commandHandler workflow journeyState)
        function
        | CreateJourney command -> createJourney workflowLookup command
        | Transition command -> transition command |> bindLookup command.JourneyId
        | ReActivate command -> reActivate command |> bindLookup command.JourneyId

    let evolve<'t> (workflow:WorkflowModel) (state:Journey<'t>) (event:JourneyEvent<'t>) =
        let ev =
          function
          | NonExistingJourney, JourneyCreated e ->
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
          | NonExistingJourney, _ -> NonExistingJourney
          | ActiveJourney s, _ -> ActiveJourney s
          | TerminatedJourney s, _ -> TerminatedJourney s
        ev (state,event)
