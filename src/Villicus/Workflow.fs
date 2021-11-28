namespace Villicus.Domain

// open Villicus.Common

type TransitionId = uint32
type StateId = uint32

type Transition = {
    Id: TransitionId
    Name: string
    SourceState: StateId
    TargetState: StateId }

type State = {
    Id: StateId
    Name: string
    Away: Set<TransitionId>
    To: Set<TransitionId>
    IsTerminal: bool
}
    with
    static member New name iD isTerminal = 
        { Id = iD; Name = name; Away = Set.empty; To = Set.empty; IsTerminal = isTerminal }

type WorkflowId = WorkflowId of System.Guid

type Version = 
    Version of uint64
    with
    member x.Inc = x |> (fun (Version i) -> i+1UL |> Version)
    
type VersionedWorkflowId =
        { Id: WorkflowId; Version: Version }
    with
        static member Decompose (x:VersionedWorkflowId) =
            let (Version version) = x.Version
            (x.Id,version)

type WorkflowEvent =
| WorkflowCreated of WorkflowNamedEvent
| WorkflowRenamed of WorkflowNamedEvent
| WorkflowCreatedAsCopy of WorkflowCreatedAsCopyEvent
| WorkflowCopied of WorkflowCopiedEvent
| WorkflowPublished of VersionedWorkflowId
| VersionIncremented of VersionedWorkflowId
| WorkflowWithdrawn of VersionedWorkflowId
| WorkflowNamed of WorkflowNamedEvent
| StateAdded of StateEditEvent
| StateRenamed of StateEditEvent
| StateDropped of StateEvent
| TerminalStateDesignated of StateEvent
| TerminalStateUnDesignated of StateEvent
| TransitionAdded of TransitionEditEvent
| TransitionChanged of TransitionEditEvent
| TransitionDropped of TransitionDroppedEvent
and WorkflowNamedEvent = { WorkflowId: WorkflowId; Name: string; }
and WorkflowCreatedAsCopyEvent = { WorkflowId: WorkflowId; Source: VersionedWorkflowId; CopyName: string }
and WorkflowCopiedEvent = { WorkflowId: WorkflowId; Version: Version; Target: WorkflowId }
and StateEditEvent = { WorkflowId: WorkflowId; StateId: StateId; StateName: string }
and StateEvent = { WorkflowId: WorkflowId; StateId: StateId }
and TransitionEditEvent = { WorkflowId: WorkflowId; TransitionId: TransitionId; TransitionName: string; SourceState: StateId; TargetState: StateId }
and TransitionDroppedEvent = { WorkflowId: WorkflowId; TransitionId: TransitionId }

type WorkflowCommand =
| CreateWorkflow of CreateWorkflowCommand
| RenameWorkflow of CreateWorkflowCommand
| CopyWorkflow of CopyWorkflowCommand
| PublishWorkflow of WorkflowId
| WithdrawWorkflow of VersionedWorkflowId
| RePublishWorkflow of VersionedWorkflowId
| AddState of AddStateCommand
| RenameState of EditStateCommand
| DropState of StateCommand
| SetTerminalState of StateCommand
| UnSetTerminalState of StateCommand
| AddTransition of AddTransitionCommand
| EditTransition of EditTransitionCommand
| DropTransition of DropTransitionCommand
and CreateWorkflowCommand = private { WorkflowId : WorkflowId; Name : string }
and CopyWorkflowCommand = private { Source: VersionedWorkflowId; Target: WorkflowId; CopyName: string }
and AddStateCommand = private { WorkflowId: WorkflowId; StateName: string }
and EditStateCommand = private { WorkflowId: WorkflowId; StateId: StateId; StateName: string }
and StateCommand = { WorkflowId: WorkflowId; StateId: StateId }
and AddTransitionCommand = private { WorkflowId: WorkflowId; TransitionName: string; SourceState: StateId; TargetState: StateId }
and EditTransitionCommand = private { WorkflowId: WorkflowId; TransitionId: TransitionId; TransitionName: string; SourceState: StateId; TargetState: StateId }
and DropTransitionCommand = { WorkflowId: WorkflowId; TransitionId: TransitionId }

type CommandCreationError =
| NullArgument of string
| CantTargetSelf of string
with
  static member ToExn =
    function
    | NullArgument s -> System.ArgumentNullException s :> exn
    | CantTargetSelf s -> System.ArgumentException s :> exn

module CommandHelpers =
    let private requireStringVal (fieldName:string) value f =
        match System.String.IsNullOrWhiteSpace value with
            | true -> NullArgument fieldName |> Error
            | false -> f () |> Ok
    let private checkDifferentStates sourceState targetState x =
        match sourceState = targetState with
        | true ->
            targetState
            |> sprintf "targetState must be different than sourceState (%O)"
            |> CantTargetSelf
            |> Error
        | false -> Ok x
    let newCreateWorkflowCommand workflowId name =
        fun () -> { WorkflowId = workflowId; Name = name }
        |> requireStringVal "name" name
    let createWorkflowCommand (c:CreateWorkflowCommand) = (c.WorkflowId, c.Name)
    let newCopyWorkflowCommand source target copyName =
        fun () -> { Source = source; Target = target; CopyName = copyName }
        |> requireStringVal "copyName" copyName
    let copyWorkflowCommand (c:CopyWorkflowCommand) = (c.Source, c.Target, c.CopyName)
    let newAddStateCommand workflowId stateName =
        fun () -> { WorkflowId = workflowId; StateName = stateName }
        |> requireStringVal "state name" stateName
    let addStateCommand (c:AddStateCommand) = (c.WorkflowId, c.StateName)
    let newEditStateCommand workflowId stateId stateName =
        fun () -> { WorkflowId = workflowId; StateId = stateId; StateName = stateName }
        |> requireStringVal "state name" stateName
    let editStateCommand (c:EditStateCommand) = (c.WorkflowId, c.StateId, c.StateName)
    let newStateCommand workflowId stateId : Result<StateCommand,CommandCreationError> =
        Ok { WorkflowId = workflowId; StateId = stateId }
    let stateCommand (c:StateCommand) = (c.WorkflowId, c.StateId)
    let newAddTransitionCommand workflowId transitionName sourceState targetState =
        fun () -> { WorkflowId = workflowId; TransitionName = transitionName; SourceState = sourceState; TargetState = targetState }
        |> requireStringVal "transition name" transitionName
        |> Result.bind (checkDifferentStates sourceState targetState)
    let addTransitionCommand (c:AddTransitionCommand) = (c.WorkflowId, c.TransitionName, c.SourceState, c.TargetState)
    let newEditTransitionCommand workflowId transitionId transitionName sourceState targetState = 
        fun () -> { WorkflowId = workflowId; TransitionId = transitionId; TransitionName = transitionName; SourceState = sourceState; TargetState = targetState }
        |> requireStringVal "transition name" transitionName
        |> Result.bind (checkDifferentStates sourceState targetState)
    let editTransitionCommand (c:EditTransitionCommand) = (c.WorkflowId, c.TransitionId, c.TransitionName, c.SourceState, c.TargetState)
    let newDropTransitionCommand workflowId transitionId : Result<DropTransitionCommand,CommandCreationError>=
        Ok { WorkflowId = workflowId; TransitionId = transitionId }
    let dropTransitionCommand (c:DropTransitionCommand) = (c.WorkflowId, c.TransitionId)

module CommandAPI =
    open CommandHelpers
    let createWorkflow workflowId = newCreateWorkflowCommand workflowId >> (Result.map CreateWorkflow)
    let renameWorkflow workflowId = newCreateWorkflowCommand workflowId >> (Result.map RenameWorkflow)
    let copyWorkflow source target = newCopyWorkflowCommand source target >> (Result.map CopyWorkflow)
    let publishWorkflow workflowId : Result<WorkflowCommand,CommandCreationError> = workflowId |> PublishWorkflow |> Ok
    let withdrawWorkflow versionedWorkflowId : Result<WorkflowCommand,CommandCreationError> = versionedWorkflowId |> WithdrawWorkflow |> Ok
    let rePublishWorkflow versionedWorkflowId : Result<WorkflowCommand,CommandCreationError> = versionedWorkflowId |> RePublishWorkflow |> Ok
    let addState workflowId = newAddStateCommand workflowId >> (Result.map AddState)
    let renameState workflowId stateId = newEditStateCommand workflowId stateId >> (Result.map RenameState)
    let addTransition workflowId transitionName sourceState = newAddTransitionCommand workflowId transitionName sourceState >> (Result.map AddTransition)
    let editTransition workflowId transitionId transitionName sourceState = newEditTransitionCommand workflowId transitionId transitionName sourceState >> (Result.map EditTransition)
    let dropState workflowId = newStateCommand workflowId >> (Result.map DropState)
    let setTerminalState workflowId = newStateCommand workflowId >> (Result.map SetTerminalState)
    let unSetTerminalState workflowId = newStateCommand workflowId >> (Result.map UnSetTerminalState)
    let dropTransition workflowId = newDropTransitionCommand workflowId >> (Result.map DropTransition)

type Problems = 
  { NoTerminalStates: bool
    UnreachableStates: Set<StateId>
    CannotReachAnyTerminalState: Set<StateId> }
  with 
    member x.Valid =
        (not x.NoTerminalStates)
        && (Set.isEmpty x.UnreachableStates)
        && (Set.isEmpty x.CannotReachAnyTerminalState)

type UndefinedTransitionError = {
    WorkflowId: WorkflowId
    UndefinedTransition: TransitionId }

type WorkflowError =
    | CommandCreation of CommandCreationError
    | MaxCountExceeded of MaxCountExceededError
    | Duplicate of WorkflowId
    | NonExistant
    | NotFound of WorkflowId
    | UndefinedVersion of VersionedWorkflowId
    | Invalid of InvalidWorkflowError
    | DuplicateStateName of DuplicateStateNameError
    | UndefinedState of UndefinedStateError
    | CantRemoveInitialState of WorkflowId
    | InitialStateCantBeTerminalState of WorkflowId
    | UndefinedTransition of UndefinedTransitionError
    | DuplicateTransition of TransitionError
    | DuplicateTransitionName of TransitionError
    | Unknown of WorkflowId * exn
and WorkflowException (message, workflowId) = 
    inherit exn(sprintf "Error for %O: %s" workflowId message)
    member __.WorkflowId = workflowId
    static member New i m = WorkflowException(m,i)
and MaxCountExceededError = {
    WorkflowId: WorkflowId
    Message: string
    MaxCountAllowed: StateId }
and InvalidWorkflowError (workflowId, problems) =
    let noneIfEmpty s = if s = "" then None else Some s
    let problemMessage p =
        let nts =
            match p.NoTerminalStates with
            | true -> Some "No Terminal States, at least one is required"
            | false -> None
        let us =
            p.UnreachableStates
            |> Set.map string
            |> Set.toSeq |> String.concat ", " |> noneIfEmpty
            |> Option.map (sprintf "States unreachable from initial state: %s")
        let crts =
            p.CannotReachAnyTerminalState
            |> Set.map string
            |> Set.toSeq |> String.concat ", " |> noneIfEmpty
            |> Option.map (sprintf "States that cannot reach a terminal state: %s")
        [ nts; us; crts]            
        |> List.choose id
        |> Seq.ofList
    member __.Message =
        workflowId.ToString ()
        |> sprintf "Workflow with id '%s' is invalid:\n"
        |> Seq.singleton |> Seq.append (problemMessage problems) |> String.concat "\n"
    member __.Problems = problems
    member __.WorkflowId = workflowId
and DuplicateStateNameError = {
    WorkflowId: WorkflowId
    StateName: string }
and UndefinedStateError = {
    WorkflowId: WorkflowId
    UndefinedState: StateId }
and TransitionError = {
    WorkflowId: WorkflowId
    ErrorTransition: Transition }
    
module WorkflowError =
    let maxCountExceeded workflowId maxCount message =
        { WorkflowId = workflowId
          Message = message
          MaxCountAllowed = maxCount }
        |> MaxCountExceeded
    let duplicate = Duplicate
    let nonExistant = NonExistant
    let undefinedVersion = UndefinedVersion
    let invalid workflowId problems =
        InvalidWorkflowError(workflowId, problems)
        |> Invalid
    let duplicateStateName workflowId stateName =
        DuplicateStateName { WorkflowId = workflowId; StateName = stateName }
    let undefinedState workflowId stateId =
        UndefinedState { WorkflowId = workflowId; UndefinedState = stateId }
    let cantRemoveInitialState = CantRemoveInitialState
    let initialStateCantBeTerminalState = InitialStateCantBeTerminalState
    let undefinedTransition workflowId transitionId =
        UndefinedTransition { WorkflowId = workflowId; UndefinedTransition = transitionId }
    let duplicateTransition workflowId transition =
        DuplicateTransition { WorkflowId = workflowId; ErrorTransition = transition }
    let duplicateTransitionName workflowId transition =
        DuplicateTransitionName { WorkflowId = workflowId; ErrorTransition = transition }
    let unknown workflowId e =
        Unknown (workflowId,e)

    
type WorkflowModel =
  { WorkflowId: WorkflowId
    Name: string
    Version: Version
    States: Map<StateId,State>
    InitialStateId: StateId
    TerminalStates: Set<StateId>
    Transitions: Map<TransitionId,Transition>
    PublishedVersions: Set<Version>
    Versions: Set<Version>
    DirectDescendents: Set<WorkflowId>
    Ancestors: VersionedWorkflowId seq }
    member x.VersionedWorkflowId = { Id = x.WorkflowId; Version = x.Version }
    member x.InitialState = Map.find x.InitialStateId x.States
    member x.StateSet = x.States |> Map.toSeq |> Seq.map fst |> Set.ofSeq
    member x.TransitionSet = x.Transitions |> Map.toSeq |> Seq.map snd |> Set.ofSeq
    member x.OrphanedStates =
        let fromTo = x.TransitionSet |> Set.map (fun t -> t.SourceState, t.TargetState)
        let initials = fromTo |> Set.map fst
        let targets = fromTo |> Set.map snd
        let orphanedTerminals = Set.difference x.TerminalStates targets
        Set.union initials targets |> Set.difference x.StateSet
        |> Set.union orphanedTerminals

type Workflow = WorkflowModel option

module Workflow =
    open WorkflowError

    let eWorkflowId = function
        | WorkflowCreated e -> e.WorkflowId
        | WorkflowRenamed e -> e.WorkflowId
        | WorkflowCreatedAsCopy e -> e.WorkflowId
        | WorkflowCopied e -> e.WorkflowId
        | WorkflowPublished e -> e.Id
        | VersionIncremented e -> e.Id
        | WorkflowWithdrawn e -> e.Id
        | WorkflowNamed e -> e.WorkflowId
        | StateAdded e -> e.WorkflowId
        | StateDropped e -> e.WorkflowId
        | StateRenamed e -> e.WorkflowId
        | TerminalStateDesignated e -> e.WorkflowId
        | TerminalStateUnDesignated e -> e.WorkflowId
        | TransitionAdded e -> e.WorkflowId
        | TransitionChanged e -> e.WorkflowId
        | TransitionDropped e -> e.WorkflowId

    let workflowId = function
        | CreateWorkflow c -> c.WorkflowId
        | RenameWorkflow c -> c.WorkflowId
        | CopyWorkflow c -> c.Target
        | PublishWorkflow c -> c
        | RePublishWorkflow c -> c.Id
        | WithdrawWorkflow c -> c.Id
        | AddState c -> c.WorkflowId
        | RenameState c -> c.WorkflowId
        | DropState c -> c.WorkflowId
        | SetTerminalState c -> c.WorkflowId
        | UnSetTerminalState c -> c.WorkflowId
        | AddTransition c -> c.WorkflowId
        | EditTransition c -> c.WorkflowId
        | DropTransition c -> c.WorkflowId

    let internal nextId typeName mp workflowId =
        let maxVal = System.UInt32.MaxValue
        let getKeys () = mp |> Map.toArray |> Array.map fst
        match Map.containsKey maxVal mp with
            | false ->  (getKeys () |> Array.last) + 1u |> Ok
            | true -> 
            let keys = getKeys () 
            // exclude the default initial state
            let minKey = keys |> Array.tail |> Array.min
            match minKey > 1u with
                | true -> minKey - 1u |> Ok
                | false ->
                keys
                |> Array.pairwise
                // looking for first gap (a prior deleted item)
                |> Array.tryFind (fun (a,b) -> b - a > 1u)
                |> Option.map (fst >> ((+) 1u) )
                //TODO should this actually be MaxVal + 1 ?
                |> Result.ofOption (sprintf "No more than %u %s's allowed" maxVal typeName
                |> maxCountExceeded workflowId maxVal)

    let internal nextStateId workFlowId workflowModel = nextId "states" workflowModel.States workFlowId
    let internal nextTransitionId workFlowId workflowModel = nextId "transitions" workflowModel.Transitions workFlowId

    let rec internal findAllTargets (foundTargets:Set<State>) (transitionsMap:Map<TransitionId,Transition>) statesMap (state:State) =
        let getDirectTargets st =
            st.Away 
            |> Set.map (fun ti -> 
                Map.find ti transitionsMap
                |> (fun t -> Map.find t.TargetState statesMap))
        let directTargets = getDirectTargets state
        let newFoundTargets = 
            Set.union directTargets foundTargets
        // skip cyclic targets
        Set.difference directTargets foundTargets
        // use Seq because Set doesn't have collect
        |> Set.toSeq |> Seq.collect (findAllTargets newFoundTargets transitionsMap statesMap >> Set.toSeq) |> Set.ofSeq
        |> Set.union directTargets
        |> Set.add state

    /// Valid Workflows meet the following criteria:
    /// - Have at least one terminal state
    /// - All states can reach at least one terminal state
    /// - Every state is reachable from the initial state
    let problems (x:WorkflowModel) =
      let reachableStates (x:WorkflowModel) =
        x.InitialState
        |> findAllTargets Set.empty x.Transitions x.States
        |> Set.map (fun y -> y.Id)
      let p =
        let reachable = reachableStates x
        { NoTerminalStates = x.TerminalStates.IsEmpty
          UnreachableStates = Set.difference x.StateSet reachable
          CannotReachAnyTerminalState =
            reachable
            |> Set.toSeq
            |> Seq.choose (fun s ->
                Map.find s x.States
                |> findAllTargets Set.empty x.Transitions x.States
                |> Set.map (fun s' -> s'.Id)
                |> Set.intersect x.TerminalStates
                |> Set.isEmpty
                |> function | true -> Some s | false -> None)
            |> Set.ofSeq }
      match p.Valid with
        | false -> Some p
        | true -> None
        
    let valid workflowModel = (problems workflowModel).IsNone

    let createWorkflow (command: CreateWorkflowCommand) state =
        match state with
        | None -> 
            let termState = State.New "Terminal State" 1u false
            [ WorkflowCreated { WorkflowId = command.WorkflowId; Name = command.Name }
              // initial state is added by creation event processing
              // this is because initial state is not an optional parameter
              StateAdded { WorkflowId = command.WorkflowId; StateId = termState.Id; StateName = termState.Name }
              TransitionAdded {
                    WorkflowId = command.WorkflowId
                    TransitionId = 0u
                    TransitionName = "Initial Transition"
                    SourceState = 0u
                    TargetState = termState.Id }
              TerminalStateDesignated { WorkflowId = command.WorkflowId; StateId = termState.Id } ]
            |> Ok
        | _ -> 
            duplicate command.WorkflowId |> Error

    let internal bindExists workflowId f : (Workflow -> Result<'a,WorkflowError>) =
        Result.ofOption NonExistant
        >> Result.bind f
    
    let renameWorkflow (command: CreateWorkflowCommand) =
        (fun s ->
            match s.Name = command.Name with
            | true -> []
            | false -> [ WorkflowRenamed { WorkflowId = command.WorkflowId; Name = command.Name } ]
            |> Ok)
        |> bindExists command.WorkflowId

    let copyWorkflow (command: CopyWorkflowCommand) =            
        fun (model:WorkflowModel) -> 
            [ [ WorkflowCopied { WorkflowId = command.Source.Id; Version = command.Source.Version; Target = command.Target }
                VersionIncremented { Id = command.Source.Id; Version = model.Version.Inc }
                WorkflowCreatedAsCopy { WorkflowId = command.Target; Source = command.Source; CopyName = command.CopyName } ]
              (model.States |> Map.toList
               |> List.map (fun (_,s) -> StateAdded { WorkflowId = command.Target; StateId = s.Id; StateName = s.Name }))
              (model.Transitions |> Map.toList
               |> List.map (fun (_,t) -> 
                 { TransitionEditEvent.WorkflowId = command.Target
                   TransitionId = t.Id
                   TransitionName = t.Name
                   SourceState = t.SourceState
                   TargetState = t.TargetState
                 } |> TransitionAdded))
              (model.TerminalStates |> Set.toList
               |> List.map (fun ts -> TerminalStateDesignated { WorkflowId = command.Target; StateId = ts }))
            ] |> List.collect id |> Ok
        |> bindExists command.Source.Id

    let internal straightPublish (command: WorkflowId) workflowModel =
        match problems workflowModel with
        | None ->
            [ WorkflowPublished { Id = command; Version = workflowModel.Version }
              VersionIncremented { Id = command; Version = workflowModel.Version.Inc } ]
            |> Ok
        | Some problems -> invalid command problems |> Error

    let publishWorkflow command = command |> straightPublish |> bindExists command

    let inline internal bindVersionExists version state x =
        match state.Versions.Contains version with
            | true -> Ok x
            | false -> undefinedVersion { Id = state.WorkflowId; Version = version } |> Error

    let rePublishWorkflow (command: VersionedWorkflowId) =
        (fun s ->
            let checkExists = bindVersionExists command.Version s
            match s.PublishedVersions.Contains command.Version with
            | true -> checkExists []
            | false -> 
                if s.Version = command.Version then
                    straightPublish command.Id s
                else
                    checkExists [ WorkflowPublished { Id = command.Id; Version = command.Version }])
        |> bindExists command.Id

    let withdrawWorkflow (command: VersionedWorkflowId) =
        (fun s ->
            match s.PublishedVersions.Contains command.Version with
              | true -> [ WorkflowWithdrawn command ] |> bindVersionExists command.Version s
              | false -> Ok [])
        |> bindExists command.Id

    let inline internal tryPickResult< ^a,^b when ^a:comparison > 
      (f: ^a -> ^b -> ^b option) (aMap:Map< ^a,^b>) createErr x = 
        Map.tryPick f aMap
        |> function
            | Some t -> t |> createErr |> Error
            | None -> Ok x

    let addState (command: AddStateCommand) =
        fun s ->
            nextStateId s.WorkflowId s
            |> Result.bind (fun n ->
                let stateEvent =
                  { StateEditEvent.WorkflowId = command.WorkflowId
                    StateId = n
                    StateName = command.StateName }
                [ StateAdded stateEvent ]
                |> tryPickResult<StateId,State>
                        (fun _ v -> match v.Name = stateEvent.StateName with | true -> Some v | false -> None)
                        s.States
                        (fun st -> duplicateStateName stateEvent.WorkflowId st.Name))
        |> bindExists command.WorkflowId

    let inline internal ifStateExists stateId x state =
        match state.States.ContainsKey stateId with
          | true -> Ok x
          | false -> undefinedState state.WorkflowId stateId |> Error

    let renameState (command: EditStateCommand) =
        [ StateRenamed { WorkflowId = command.WorkflowId; StateId = command.StateId; StateName = command.StateName } ]
        |> ifStateExists command.StateId
        |> bindExists command.WorkflowId

    let inline internal initialStateErr command errFunc x =
        match command.StateId <> 0u with
          | true -> Ok x
          | false -> errFunc command.WorkflowId |> Error

    let dropState (command: StateCommand) = 
        [ StateDropped { WorkflowId = command.WorkflowId; StateId = command.StateId } ]
        |> ifStateExists command.StateId
        >> Result.bind (initialStateErr command cantRemoveInitialState)
        |> bindExists command.WorkflowId

    let setTerminalState (command: StateCommand ) =
        fun s ->
            let events =
                match s.TerminalStates.Contains command.StateId with
                  | true -> []
                  | false -> [ TerminalStateDesignated { WorkflowId = command.WorkflowId; StateId = command.StateId } ]
            s |> ifStateExists command.StateId events
            |> Result.bind (initialStateErr command initialStateCantBeTerminalState)
        |> bindExists command.WorkflowId

    let unSetTerminalState (command: StateCommand ) =
        fun s ->
            let events =
                match s.TerminalStates.Contains command.StateId with
                  | true -> [ TerminalStateUnDesignated { WorkflowId = command.WorkflowId; StateId = command.StateId } ]
                  | false -> []
            s |> ifStateExists command.StateId events
        |> bindExists command.WorkflowId

    let internal transitionCheck workflowModel (transEvent:TransitionEditEvent) x =
        let transitionNameExists _ (v:Transition) =
            match v.Name = transEvent.TransitionName with | true -> Some v | false -> None
        let transitionPathExists _ (v:Transition) =
            match v.SourceState = transEvent.SourceState && v.TargetState = transEvent.TargetState with | true -> Some v | false -> None
        x
        |> tryPickResult<TransitionId,Transition> transitionNameExists workflowModel.Transitions (fun t -> duplicateTransitionName transEvent.WorkflowId t)
        |> Result.bind (tryPickResult<TransitionId,Transition> transitionPathExists workflowModel.Transitions (fun t -> duplicateTransition transEvent.WorkflowId t))
        |> Result.bind (fun y -> ifStateExists transEvent.SourceState y workflowModel)
        |> Result.bind (fun y -> ifStateExists transEvent.TargetState y workflowModel)

    let addTransition (command: AddTransitionCommand) =
        fun s ->
            nextTransitionId command.WorkflowId s
            |> Result.bind (fun n ->
                let editEvent = {
                    TransitionEditEvent.WorkflowId = command.WorkflowId
                    TransitionId = n
                    TransitionName = command.TransitionName
                    SourceState = command.SourceState 
                    TargetState = command.TargetState }
                [ TransitionAdded editEvent ]
                |> transitionCheck s editEvent)
        |> bindExists command.WorkflowId

    let internal ifTransitionExists transitionId x state =
        match state.Transitions.ContainsKey transitionId with
        | true -> Ok x
        | false -> undefinedTransition state.WorkflowId transitionId |> Error

    let editTransition (command: EditTransitionCommand) =
        fun s ->
            let editEvent = {
                TransitionEditEvent.WorkflowId = command.WorkflowId
                TransitionId = command.TransitionId
                TransitionName = command.TransitionName
                SourceState = command.SourceState 
                TargetState = command.TargetState }            
            s |> ifTransitionExists command.TransitionId [ TransitionChanged editEvent ]
            |> Result.bind (transitionCheck { s with Transitions = s.Transitions |> Map.remove command.TransitionId } editEvent)
        |> bindExists command.WorkflowId

    let dropTransition (command: DropTransitionCommand) =
        [ TransitionDropped { WorkflowId = command.WorkflowId; TransitionId = command.TransitionId } ]
        |> ifTransitionExists command.TransitionId
        |> bindExists command.WorkflowId

    let handle =
        function
        | CreateWorkflow command -> createWorkflow command
        | RenameWorkflow command -> renameWorkflow command
        | CopyWorkflow command -> copyWorkflow command
        | PublishWorkflow command -> publishWorkflow command
        | RePublishWorkflow command -> rePublishWorkflow command
        | WithdrawWorkflow command -> withdrawWorkflow command
        | AddState command -> addState command
        | RenameState command -> renameState command
        | DropState command -> dropState command
        | SetTerminalState command -> setTerminalState command
        | UnSetTerminalState command -> unSetTerminalState command
        | AddTransition command -> addTransition command
        | EditTransition command -> editTransition command
        | DropTransition command -> dropTransition command

    let evolve (state:Workflow) event : Workflow =
        let createNew workflowId name =
            let initialState = State.New "Initial State" 0u false
            let initialVersion = Version 0UL
            { WorkflowId = workflowId
              Version = initialVersion
              States = Map.empty<StateId,State> |> Map.add initialState.Id initialState 
              InitialStateId = initialState.Id
              TerminalStates = Set.empty<StateId>
              Transitions = Map.empty<TransitionId,Transition>
              Name = name
              PublishedVersions = Set.empty<Version>
              Versions = Set.singleton initialVersion
              DirectDescendents = Set.empty<WorkflowId>
              Ancestors = Seq.empty<VersionedWorkflowId> }
        let remTransition transitionId s =
            let oldTran = s.Transitions |> Map.find transitionId
            let remSource = 
                let t = s.States |> Map.find oldTran.SourceState
                { t with Away = t.Away |> Set.remove oldTran.Id }
            let remTarget = 
                let t = s.States |> Map.find oldTran.TargetState
                { t with To = t.To |> Set.remove oldTran.Id }
            { s with States = s.States |> Map.add remSource.Id remSource |> Map.add remTarget.Id remTarget}
        let addTransition (e:TransitionEditEvent) s =
            let newTran = {
                Id = e.TransitionId
                Name = e.TransitionName
                SourceState = e.SourceState
                TargetState = e.TargetState }
            let addSource = 
                let t = s.States |> Map.find e.SourceState
                { t with Away = t.Away |> Set.add newTran.Id }
            let addTarget = 
                let t = s.States |> Map.find e.TargetState
                { t with To = t.To |> Set.add newTran.Id }
            { s with 
                Transitions = s.Transitions |> Map.add newTran.Id newTran
                States = s.States |> Map.add addSource.Id addSource |> Map.add addTarget.Id addTarget}
        let procEvent (s:WorkflowModel) =
            function
            | WorkflowCreated _ -> s
            | WorkflowRenamed e -> { s with Name = e.Name }
            | WorkflowCreatedAsCopy e ->
              { s with
                  WorkflowId = e.WorkflowId 
                  Ancestors = seq { yield! s.Ancestors; yield e.Source } }
            | WorkflowCopied e ->
              { s with
                  DirectDescendents = Set.add e.Target s.DirectDescendents }
            | WorkflowPublished e -> { s with PublishedVersions = s.PublishedVersions |> Set.add e.Version }
            | VersionIncremented e -> { s with Version = e.Version; Versions = s.Versions |> Set.add e.Version }
            | WorkflowWithdrawn e -> { s with PublishedVersions = s.PublishedVersions |> Set.remove e.Version }
            | WorkflowNamed e -> { s with Name = e.Name }
            | StateAdded e -> { s with States = Map.add e.StateId (State.New e.StateName e.StateId false) s.States }
            | StateRenamed e ->
              let st = s.States |> Map.find e.StateId 
              { s with States = s.States |> Map.add e.StateId { st with Name = e.StateName } }
            | StateDropped e ->
              { s with 
                  States = Map.remove e.StateId s.States 
                  Transitions = s.Transitions |> Map.filter (fun _ t -> t.SourceState <> e.StateId && t.TargetState <> e.StateId) }
            | TerminalStateDesignated e ->
              let termState = { (s.States |> Map.find e.StateId) with IsTerminal = true }
              { s with 
                  TerminalStates = s.TerminalStates |> Set.add e.StateId
                  States = s.States |> Map.add e.StateId termState }
            | TerminalStateUnDesignated e ->
              let nonTermState = { (s.States |> Map.find e.StateId) with IsTerminal = false }
              { s with 
                  TerminalStates = s.TerminalStates |> Set.remove e.StateId
                  States = s.States |> Map.add e.StateId nonTermState }
            | TransitionAdded e -> addTransition e s
            | TransitionChanged e -> remTransition e.TransitionId s |> addTransition e
            | TransitionDropped e ->
              let s' = remTransition e.TransitionId s
              { s' with Transitions = s.Transitions |> Map.remove e.TransitionId }
        match (state, event) with
        //We are ignoring possible error where WorkflowCreated event on Some state
        //However, this is caught by not allowing create command on Some
        | None, WorkflowCreated e -> createNew e.WorkflowId e.Name |> Some
        | None, _ -> None
        | Some s, event -> procEvent s event |> Some

    type CommandProcessor<'eventId> =
        Sentinam.CommandProcessor<Workflow,WorkflowId,WorkflowCommand,WorkflowEvent,'eventId,WorkflowError> 

    type ReadStreamReply<'eventId> = Sentinam.ReadStreamReply<WorkflowEvent,'eventId,WorkflowError>

    type ReadStream<'eventId> = Sentinam.ReadStream<WorkflowId,WorkflowEvent,'eventId,WorkflowError>

    type Envelope<'eventId> = Sentinam.Envelope<WorkflowCommand,WorkflowEvent,'eventId,WorkflowId,Workflow,WorkflowError>

    type ResultCommand<'eventId> = Sentinam.ResultCommand<WorkflowCommand,Workflow,'eventId,WorkflowError>


