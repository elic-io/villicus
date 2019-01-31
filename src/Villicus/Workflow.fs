namespace Villicus.Domain

open Villicus.Common

type TransitionId = uint32
type StateId = uint32

type Transition = {
    Id: TransitionId
    Name: string
    SourceState: StateId
    TargetState: StateId
}

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
    
type VersionedWorkflowId = { Id: WorkflowId; Version: Version }
type WorkflowEvent =
| WorkflowCreated of WorkflowNamedEvent
| WorkflowRenamed of WorkflowNamedEvent
| WorkflowCreatedAsCopy of WorkflowCreatedAsCopyEvent
| WorkflowCopied of WorkflowCopiedEvent
| WorkflowPublished of VersionedWorkflowEvent
| VersionIncremented of VersionedWorkflowEvent
| WorkflowWithdrawn of VersionedWorkflowEvent
| WorkflowNamed of WorkflowNamedEvent
| StateAdded of StateEditEvent
| StateDropped of StateEvent
| StateRenamed of StateEditEvent
| TerminalStateDesignated of StateEvent
| TerminalStateUnDesignated of StateEvent
| TransitionAdded of TransitionEditEvent
| TransitionChanged of TransitionEditEvent
| TransitionDropped of TransitionDroppedEvent
and WorkflowNamedEvent = { WorkflowId: WorkflowId; Name: string; }
and VersionedWorkflowEvent = VersionedWorkflowId
and WorkflowCreatedAsCopyEvent = { WorkflowId: WorkflowId; Source: VersionedWorkflowId; CopyName: string }
and WorkflowCopiedEvent = { WorkflowId: WorkflowId; Version: Version; Target: WorkflowId }
and StateEditEvent = { WorkflowId: WorkflowId; StateId: StateId; StateName: string }
and StateEvent = { WorkflowId: WorkflowId; StateId: StateId }
and TransitionEditEvent = { WorkflowId: WorkflowId; TransitionId: TransitionId; TransitionName: string; SourceState: StateId; TargetState: StateId }
and TransitionDroppedEvent = { WorkflowId: WorkflowId; TransitionId: TransitionId }


type CreateWorkflowCommand (workflowId, name) =
    do
        requireStringVal "name" name
    member __.WorkflowId = workflowId
    member __.Name = name

type CopyWorkflowCommand (source, target, copyName) =
    do
        requireStringVal "copyName" copyName
    member __.Source = source
    member __.Target = target
    member __.CopyName = copyName

type AddStateCommand (workflowId, stateName) =
    do
        requireStringVal "state name" stateName
    member __.WorkflowId = workflowId
    member __.StateName = stateName

type EditStateCommand (workflowId, stateId, stateName) =
    inherit AddStateCommand(workflowId, stateName)
    member __.StateId = stateId

type AddTransitionCommand (workflowId, transitionName, initialState, targetState) = 
    do
        requireStringVal "transition name" transitionName
        match initialState = targetState with
            | true ->
            let errMsg = targetState |> sprintf "targetState must be different than initialState (%O)"
            raise (System.ArgumentException(errMsg,"targetState"))
            | false -> ()
    member __.WorkflowId = workflowId
    member __.TransitionName = transitionName
    member __.InitialState = initialState
    member __.TargetState = targetState

type EditTransitionCommand (workflowId, transitionId, transitionName, initialState, targetState) = 
    inherit AddTransitionCommand (workflowId, transitionName, initialState, targetState)
    member __.TransitionId = transitionId

type WorkflowCommand =
| CreateWorkflow of CreateWorkflowCommand
| RenameWorkflow of CreateWorkflowCommand
| CopyWorkflow of CopyWorkflowCommand
| PublishWorkflow of WorkflowId
| RePublishWorkflow of VersionedWorkflowId 
| WithdrawWorkflow of VersionedWorkflowId
| AddState of AddStateCommand
| RenameState of EditStateCommand
| DropState of StateCommand
| SetTerminalState of StateCommand
| UnSetTerminalState of StateCommand
| AddTransition of AddTransitionCommand
| EditTransition of EditTransitionCommand
| DropTransition of DropTransitionCommand
and StateCommand = { WorkflowId: WorkflowId; State: StateId }
and DropTransitionCommand = { WorkflowId: WorkflowId; Transition: TransitionId }

type Problems = 
    {
    NoTerminalStates: bool
    UnreachableStates: Set<StateId>
    CannotReachAnyTerminalState: Set<StateId> }
  with 
    member x.Valid =
        (not x.NoTerminalStates)
        && (Set.isEmpty x.UnreachableStates)
        && (Set.isEmpty x.CannotReachAnyTerminalState)

type WorkflowException (message, workflowId) = 
    inherit exn(sprintf "Error for %O: %s" workflowId message)
    member __.WorkflowId = workflowId
    static member New i m = WorkflowException(m,i)
    
type MaxCountExceededException (message, workflowId, maxCountAllowed) = 
    inherit WorkflowException(message, workflowId)
  with
    member __.MaxCountAllowed = maxCountAllowed

type DuplicateWorkflowIdException (workflowId) = 
    inherit WorkflowException("already exists",workflowId)

type NonExistantWorkflowException (workflowId) =
    inherit WorkflowException("workflow does not exist", workflowId)

type UndefinedVersionException (workflowId,version) =
    inherit WorkflowException(sprintf "%O not found" version,workflowId)
    with
    member __.Version = version
    static member New w v = UndefinedVersionException(w,v)

type InvalidWorkflowException (message, workflowId, problems) = 
    inherit WorkflowException("",workflowId)
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
    override __.Message =
        sprintf "%s%s" base.Message message
        |> Seq.singleton |> Seq.append (problemMessage problems) |> String.concat "\n"
    member __.Problems = problems

type DuplicateStateNameException (workflowId,stateName) =
    inherit WorkflowException(sprintf "State with name '%s' already exists" stateName,workflowId)
  with
    member __.StateName = stateName
    static member New i s = DuplicateStateNameException(i,s)

type UndefinedStateException (workflowId,stateId) =
    inherit WorkflowException(sprintf "%O not found" stateId,workflowId)
  with
    member __.StateId = stateId

type InitialStateException (message,workflowId) = inherit WorkflowException(message,workflowId)

type UndefinedTransitionException (workflowId,transitionId) =
    inherit WorkflowException(sprintf "%O not found" transitionId,workflowId)
  with
    member __.TransitionId = transitionId

type DuplicateTransitionException (workflowId,transition:Transition) =
    inherit WorkflowException(sprintf "%O with name '%s' already exists between source '%O' and target '%O'" transition.Id transition.Name transition.SourceState transition.TargetState,workflowId)
  with
    member __.Transition = transition

type DuplicateTransitionNameException (workflowId,transition:Transition) =
    inherit WorkflowException(sprintf "%O with unique name '%s' already exists" transition.Id transition.Name,workflowId)
  with
    member __.Transition = transition
    
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

    let eWorkflowId = function
        | WorkflowCreated e -> e.WorkflowId
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
                |> Result.ofOption (sprintf "No more than %u %s's allowed" maxVal typeName |> MaxCountExceededException.New workflowId  :> exn)

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
                    TransitionName = "Transition"
                    SourceState = 0u
                    TargetState = termState.Id }
              TerminalStateDesignated { WorkflowId = command.WorkflowId; StateId = termState.Id } ]
            |> Ok
        | _ -> 
            DuplicateWorkflowIdException(command.WorkflowId) :> exn |> Error

    let internal bindExists workflowId f : (Workflow -> Result<'a,exn>) =
        Result.ofOption (workflowId |> NonExistantWorkflowException :> exn)
        >> Result.bind f
    
    let renameWorkflow (command: CreateWorkflowCommand) state =
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
                 { WorkflowId = command.Target
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
        | Some problems ->
            (sprintf "%O is not valid and cannot be published" workflowModel.Version,command,problems)
            |> InvalidWorkflowException :> exn |> Error

    let publishWorkflow command = command |> straightPublish |> bindExists command

    let inline internal bindVersionExists version state x =
        match state.Versions.Contains version with
            | true -> Ok x
            | false -> UndefinedVersionException (state.WorkflowId,version) :> exn |> Error

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
            | Some t -> t |> createErr :> exn |> Error
            | None -> Ok x

    let addState (command: AddStateCommand) =
        fun s ->
            nextStateId s.WorkflowId s
            |> Result.bind (fun n ->
                let stateEvent =
                  { WorkflowId = command.WorkflowId
                    StateId = n
                    StateName = command.StateName }
                [ StateAdded stateEvent ]
                |> tryPickResult<StateId,State>
                        (fun _ v -> match v.Name = stateEvent.StateName with | true -> Some v | false -> None)
                        s.States
                        (fun st -> DuplicateStateNameException(stateEvent.WorkflowId,st.Name) :> exn))
        |> bindExists command.WorkflowId

    let inline internal ifStateExists stateId x state =
        match state.States.ContainsKey stateId with
          | true -> Ok x
          | false -> UndefinedStateException(state.WorkflowId,stateId) :> exn |> Error

    let renameState (command: EditStateCommand) =
        [ StateRenamed { WorkflowId = command.WorkflowId; StateId = command.StateId; StateName = command.StateName } ]
        |> ifStateExists command.StateId
        |> bindExists command.WorkflowId

    let inline internal initialStateError command errMsg x =
        match command.State <> 0u with
          | true -> Ok x
          | false -> InitialStateException(errMsg,command.WorkflowId) :> exn |> Error

    let dropState (command: StateCommand) = 
        [ StateDropped { WorkflowId = command.WorkflowId; StateId = command.State } ]
        |> ifStateExists command.State
        >> Result.bind (initialStateError command "Initial state cannot be removed")
        |> bindExists command.WorkflowId

    let setTerminalState (command: StateCommand ) =
        fun s ->
            let events =
                match s.TerminalStates.Contains command.State with
                  | true -> []
                  | false -> [ TerminalStateDesignated { WorkflowId = command.WorkflowId; StateId = command.State } ]
            s |> ifStateExists command.State events
            |> Result.bind (initialStateError command "Initial state cannot be a terminal state")
        |> bindExists command.WorkflowId

    let unSetTerminalState (command: StateCommand ) =
        fun s ->
            let events =
                match s.TerminalStates.Contains command.State with
                  | true -> [ TerminalStateUnDesignated { WorkflowId = command.WorkflowId; StateId = command.State } ]
                  | false -> []
            s |> ifStateExists command.State events
        |> bindExists command.WorkflowId

    let internal transitionCheck workflowModel transEvent x =
        let transitionNameExists _ (v:Transition) =
            match v.Name = transEvent.TransitionName with | true -> Some v | false -> None
        let transitionPathExists _ (v:Transition) =
            match v.SourceState = transEvent.SourceState && v.TargetState = transEvent.TargetState with | true -> Some v | false -> None
        x
        |> tryPickResult<TransitionId,Transition> transitionNameExists workflowModel.Transitions (fun t -> DuplicateTransitionNameException(transEvent.WorkflowId,t) :> exn)
        |> Result.bind (tryPickResult<TransitionId,Transition> transitionPathExists workflowModel.Transitions (fun t -> DuplicateTransitionException(transEvent.WorkflowId,t) :> exn))
        |> Result.bind (fun y -> ifStateExists transEvent.SourceState y workflowModel)
        |> Result.bind (fun y -> ifStateExists transEvent.TargetState y workflowModel)

    let addTransition (command: AddTransitionCommand) =
        fun s ->
            nextTransitionId command.WorkflowId s
            |> Result.bind (fun n ->
                let editEvent = {
                    WorkflowId = command.WorkflowId
                    TransitionId = n
                    TransitionName = command.TransitionName
                    SourceState = command.InitialState 
                    TargetState = command.TargetState }
                [ TransitionAdded editEvent ]
                |> transitionCheck s editEvent)
        |> bindExists command.WorkflowId

    let internal ifTransitionExists transitionId x state =
        match state.Transitions.ContainsKey transitionId with
        | true -> Ok x
        | false -> UndefinedTransitionException(state.WorkflowId,transitionId) :> exn |> Error

    let editTransition (command: EditTransitionCommand) =
        fun s ->
            let editEvent = {
                WorkflowId = command.WorkflowId
                TransitionId = command.TransitionId
                TransitionName = command.TransitionName
                SourceState = command.InitialState 
                TargetState = command.TargetState }            
            s |> ifTransitionExists command.TransitionId [ TransitionChanged editEvent ]
            |> Result.bind (transitionCheck { s with Transitions = s.Transitions |> Map.remove command.TransitionId } editEvent)
        |> bindExists command.WorkflowId

    let dropTransition (command: DropTransitionCommand) =
        [ TransitionDropped { WorkflowId = command.WorkflowId; TransitionId = command.Transition } ]
        |> ifTransitionExists command.Transition
        |> bindExists command.WorkflowId

    let handle = 
        function
        | CreateWorkflow command -> createWorkflow command
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
            
        let ev =
          function
          //We are ignoring possible error where WorkflowCreated event on Some state
          //However, this is caught by not allowing create command on Some
          | None, WorkflowCreated e -> createNew e.WorkflowId e.Name |> Some
          | None, _ -> None
          | Some _, WorkflowCreated _ -> None
          | Some s, WorkflowRenamed e ->Some { s with Name = e.Name }
          | Some s, WorkflowCreatedAsCopy e ->
            { s with
                WorkflowId = e.WorkflowId 
                Ancestors = seq { yield! s.Ancestors; yield e.Source } }
            |> Some
          | Some s, WorkflowCopied e ->
            { s with
                DirectDescendents = Set.add e.Target s.DirectDescendents }
            |> Some
          | Some s, WorkflowPublished e -> Some { s with PublishedVersions = s.PublishedVersions |> Set.add e.Version }
          | Some s, VersionIncremented e -> Some { s with Version = e.Version; Versions = s.Versions |> Set.add e.Version }
          | Some s, WorkflowWithdrawn e -> Some { s with PublishedVersions = s.PublishedVersions |> Set.remove e.Version }
          | Some s, WorkflowNamed e -> Some { s with Name = e.Name }
          | Some s, StateAdded e -> Some { s with States = Map.add e.StateId (State.New e.StateName e.StateId false) s.States }
          | Some s, StateRenamed e ->
            let st = s.States |> Map.find e.StateId 
            Some { s with States = s.States |> Map.add e.StateId { st with Name = e.StateName } }
          | Some s, StateDropped e ->
            { s with 
                States = Map.remove e.StateId s.States 
                Transitions = s.Transitions |> Map.filter (fun _ t -> t.SourceState <> e.StateId && t.TargetState <> e.StateId) }
            |> Some
          | Some s, TerminalStateDesignated e ->
            let termState = { (s.States |> Map.find e.StateId) with IsTerminal = true }
            { s with 
                TerminalStates = s.TerminalStates |> Set.add e.StateId
                States = s.States |> Map.add e.StateId termState } |> Some
          | Some s, TerminalStateUnDesignated e ->
            let nonTermState = { (s.States |> Map.find e.StateId) with IsTerminal = false }
            { s with 
                TerminalStates = s.TerminalStates |> Set.remove e.StateId
                States = s.States |> Map.add e.StateId nonTermState } |> Some 
          | Some s, TransitionAdded e -> addTransition e s |> Some
          | Some s, TransitionChanged e -> remTransition e.TransitionId s |> addTransition e |> Some
          | Some s, TransitionDropped e ->
            let s' = remTransition e.TransitionId s
            Some { s' with Transitions = s.Transitions |> Map.remove e.TransitionId }
        ev (state,event)
