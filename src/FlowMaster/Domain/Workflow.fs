namespace FlowMaster.Domain

/// Command Interface Module
///
/// ## Command Interface Module
///
///     let h = Library.hello 1
///     printfn "%d" h
///
module Workflow =
    type TransitionId = Transition of System.Guid
    type StateId = StateId of System.Guid

    type Transition = {
        Id: TransitionId
        Name: string
        InitialState: StateId
        TargetState: StateId
    }

    type WorkflowId = WorkflowId of System.Guid
    type Version = Version of uint64
    type VersionedWorkflowId = { Id: WorkflowId; Version: Version }
    // type Workflow =
    // | Id of WorkflowId
    // | VersionedId of VersionedWorkflowId

    /// Returns 42
    ///
    /// ## Parameters
    ///  - `num` - whatever
    type Event =
    | WorkflowCreated of WorkflowEvent
    | WorkflowCreatedAsCopy of WorkflowCreatedAsCopyEvent
    | WorkflowCopied of WorkflowCopiedEvent
    | WorkflowPublished of VersionedWorkflowEvent
    | VersionIncremented of VersionedWorkflowEvent
    | WorkflowWithdrawn of VersionedWorkflowEvent
    | WorkflowNamed of WorkflowNamedEvent
    | StateAdded of StateEditEvent
    | StateDropped of StateEvent
    | StateRenamed of StateEditEvent
    | InitialStateDesignated of StateEvent
    | TerminalStateDesignated of StateEvent
    | TerminalStateUnDesignated of StateEvent
    | TransitionAdded of TransitionEditEvent
    | TransitionChanged of TransitionEditEvent
    | TransitionDropped of TransitionEvent
    and WorkflowEvent = WorkflowId
    and VersionedWorkflowEvent = VersionedWorkflowId
    and WorkflowCreatedAsCopyEvent = { WorkflowId: WorkflowId; Source: VersionedWorkflowId }
    and WorkflowCopiedEvent = { WorkflowId: WorkflowId; Version: Version; Target: WorkflowId }
    and WorkflowNamedEvent = { WorkflowId: WorkflowId; Name: string; }
    and StateEditEvent = { WorkflowId: WorkflowId; StateId: StateId; StateName: string }
    and StateEvent = { WorkflowId: WorkflowId; StateId: StateId }
    and TransitionEditEvent = { WorkflowId: WorkflowId; TransitionId: TransitionId; TransitionName: string; InitialState: StateId; TargetState: StateId }
    and TransitionEvent = { WorkflowId: WorkflowId; TransitionId: TransitionId }
  
    type Command =
    | CreateWorkflow of WorkflowId
    | CopyWorkflow of CopyCommand
    | PublishWorkflow of VersionedWorkflowId
    | WithdrawWorkflow of VersionedWorkflowId
    | AddState of EditStateCommand
    | RenameState of EditStateCommand
    | DropState of StateCommand
    | SetInitialState of StateCommand
    | SetTerminalState of StateCommand
    | UnSetTerminalState of StateCommand
    | AddTransition of EditTransitionCommand
    | EditTransition of EditTransitionCommand
    | DropTransition of TransitionCommand
    and StateCommand = { WorkflowId: WorkflowId; State: StateId }
    and CopyCommand = { WorkflowId: WorkflowId; Source: VersionedWorkflowId }
    and EditStateCommand = { WorkflowId: WorkflowId; State: StateId; StateName: string }
    and TransitionCommand = { WorkflowId: WorkflowId; Transition: TransitionId }
    and EditTransitionCommand = { WorkflowId: WorkflowId; Transition: TransitionId; TransitionName: string; InitialState: StateId; TargetState: StateId }

    let workflowId = function
        | CreateWorkflow c -> c
        | CopyWorkflow c -> c.WorkflowId
        | PublishWorkflow c -> c.Id
        | WithdrawWorkflow c -> c.Id
        | AddState c -> c.WorkflowId
        | RenameState c -> c.WorkflowId
        | DropState c -> c.WorkflowId
        | SetInitialState c -> c.WorkflowId
        | SetTerminalState c -> c.WorkflowId
        | UnSetTerminalState c -> c.WorkflowId
        | AddTransition c -> c.WorkflowId
        | EditTransition c -> c.WorkflowId
        | DropTransition c -> c.WorkflowId

    // let version = function
    //     | WorkflowCreated _ -> Version 0UL
    //     | WorkflowCreatedAsCopy _ -> Version 0UL
    //     | WorkflowCopied e -> e.Version 
    //     | WorkflowPublished e -> e.Version
    //     | VersionIncremented e -> e.Version
    //     | WorkflowWithdrawn e -> e.Version
    //     | WorkflowNamed _ -> Version 0UL
    //     | StateAdded _ -> Version 0UL
    //     | StateDropped _ -> Version 0UL
    //     | StateRenamed _ -> Version 0UL
    //     | InitialStateDesignated _ -> Version 0UL
    //     | TerminalStateDesignated _ -> Version 0UL
    //     | TerminalStateUnDesignated _ -> Version 0UL
    //     | TransitionAdded _ -> Version 0UL
    //     | TransitionChanged _ -> Version 0UL
    //     | TransitionDropped _ -> Version 0UL

    type WorkflowState = 
    | NonExistant
    | UnInitialized of WorkflowModel
    | Initialized of WorkflowModel

    and WorkflowModel = {
        WorkflowId: WorkflowId
        Version: Version
        States: Map<StateId,string>
        InitialState: StateId option
        TerminalStates: Set<StateId> 
        Transitions: Map<TransitionId,Transition>
        Name: string
        PublishedVersions: Set<Version>
        DirectDescendents: Set<WorkflowId>
        Ancestors: VersionedWorkflowId seq }

    let createWorkflow (command: WorkflowId) state =
        match state with
        | NonExistant -> Ok [ WorkflowCreated command ]
        | _ -> 
            sprintf "Workflow with Id %O already exists" command 
            |> Error

    let copyWorkflow (command: CopyCommand) state =
        match state with
        | NonExistant -> 
            [ WorkflowCreatedAsCopy { WorkflowId = command.WorkflowId; Source = command.Source }
              WorkflowCopied { WorkflowId = command.Source.Id; Version = command.Source.Version; Target = command.WorkflowId } ]
            |> Ok
        | _ -> 
            sprintf "Workflow with Id %O already exists" command.WorkflowId
            |> Error

    let publishWorkflow (command: VersionedWorkflowId) state =
        let incVersion v =
            match v with
            | Version i -> i+1UL |> Version
        match state with
        | NonExistant -> sprintf "Workflow Id %O does not exist" command.Id |> Error
        | _ -> Ok [ WorkflowPublished command; VersionIncremented { command with Version = incVersion command.Version } ]

    let withdrawWorkflow (command: VersionedWorkflowId) state =
        match state with
        | NonExistant -> sprintf "Workflow Id %O does not exist" command.Id |> Error
        | _ -> Ok [ WorkflowWithdrawn command ]

    let addState (command: EditStateCommand) state =
        match state with 
        | NonExistant -> sprintf "Workflow Id %O does not exist" command.WorkflowId |> Error
        | _ -> Ok [ StateAdded { WorkflowId = command.WorkflowId; StateId = command.State; StateName = command.StateName } ]
    
    let renameState (command: EditStateCommand) state =
        match state with 
        | NonExistant -> sprintf "Workflow Id %O does not exist" command.WorkflowId |> Error
        | _ -> Ok [ StateRenamed { WorkflowId = command.WorkflowId; StateId = command.State; StateName = command.StateName } ]

    let dropState (command: StateCommand) state = 
        match state with 
        | NonExistant -> sprintf "Workflow Id %O does not exist" command.WorkflowId |> Error
        | _ -> Ok [ StateDropped { WorkflowId = command.WorkflowId; StateId = command.State } ]

    let setInitialState (command: StateCommand ) state =
        match state with 
        | NonExistant -> sprintf "Workflow Id %O does not exist" command.WorkflowId |> Error
        | _ -> Ok [ InitialStateDesignated { WorkflowId = command.WorkflowId; StateId = command.State } ]

    let setTerminalState (command: StateCommand ) state =
        match state with 
        | NonExistant -> sprintf "Workflow Id %O does not exist" command.WorkflowId |> Error
        | _ -> Ok [ TerminalStateDesignated { WorkflowId = command.WorkflowId; StateId = command.State } ]

    let unSetTerminalState (command: StateCommand ) state =
        match state with 
        | NonExistant -> sprintf "Workflow Id %O does not exist" command.WorkflowId |> Error
        | _ -> Ok [ TerminalStateUnDesignated { WorkflowId = command.WorkflowId; StateId = command.State } ]

    let addTransition (command: EditTransitionCommand) state =
        match state with 
        | NonExistant -> sprintf "Workflow Id %O does not exist" command.WorkflowId |> Error
        | _ -> 
            [ TransitionAdded {
                WorkflowId = command.WorkflowId
                TransitionId = command.Transition
                TransitionName = command.TransitionName
                InitialState = command.InitialState 
                TargetState = command.TargetState
            } ] |> Ok
    let editTransition (command: EditTransitionCommand) state =
        match state with 
        | NonExistant -> sprintf "Workflow Id %O does not exist" command.WorkflowId |> Error
        | _ -> 
            [ TransitionChanged {
                WorkflowId = command.WorkflowId
                TransitionId = command.Transition
                TransitionName = command.TransitionName
                InitialState = command.InitialState 
                TargetState = command.TargetState
            } ] |> Ok
    let dropTransition (command: TransitionCommand) state =
        match state with 
        | NonExistant -> sprintf "Workflow Id %O does not exist" command.WorkflowId |> Error
        | _ -> Ok [ TransitionDropped { WorkflowId = command.WorkflowId; TransitionId = command.Transition } ]

    let handle = 
        function
            | CreateWorkflow command -> createWorkflow command
            | CopyWorkflow command -> copyWorkflow command
            | PublishWorkflow command -> publishWorkflow command
            | WithdrawWorkflow command -> withdrawWorkflow command
            | AddState command -> addState command
            | RenameState command -> renameState command
            | DropState command -> dropState command
            | SetInitialState command -> setInitialState command
            | SetTerminalState command -> setTerminalState command
            | UnSetTerminalState command -> unSetTerminalState command
            | AddTransition command -> addTransition command
            | EditTransition command -> editTransition command
            | DropTransition command -> dropTransition command
            
    type WorkflowState with
        static member Evolve state = 
            let transformModel (ws:WorkflowState) (t:WorkflowModel->WorkflowModel) =
                match ws with
                | NonExistant -> NonExistant
                | UnInitialized s -> t s |> UnInitialized
                | Initialized s -> t s |> Initialized

            function
            | WorkflowCreated event -> 
              { WorkflowId = event
                Version = Version 0UL
                States = Map.empty<StateId,string>
                TerminalStates = Set.empty<StateId>
                InitialState = None
                Transitions = Map.empty<TransitionId,Transition>
                Name = "UnNamed"
                PublishedVersions = Set.empty<Version>
                DirectDescendents = Set.empty<WorkflowId>
                Ancestors = Seq.empty<VersionedWorkflowId> }
              |> UnInitialized
            | WorkflowCreatedAsCopy event -> NonExistant
            | WorkflowCopied event -> NonExistant
            | WorkflowPublished event ->
                let pub s:WorkflowModel = 
                    { s with Version = event.Version; PublishedVersions = s.PublishedVersions |> Set.add event.Version }
                transformModel state pub
            | VersionIncremented event ->
                let vInc s:WorkflowModel = { s with Version = event.Version }
                transformModel state vInc
            | WorkflowWithdrawn event ->
                let unpub s:WorkflowModel = { s with PublishedVersions = s.PublishedVersions |> Set.add event.Version }
                transformModel state unpub
            | WorkflowNamed event ->
                let changeName s:WorkflowModel = { s with Name = event.Name }
                transformModel state changeName
            | StateAdded event ->
                let addSt s = { s with States = Map.add event.StateId event.StateName s.States }
                transformModel state addSt
            | StateDropped event ->
                let dropSt s = { s with States = Map.remove event.StateId s.States }
                transformModel state dropSt
            | StateRenamed event ->
                let renameSt s = { s with States = s.States |> Map.remove event.StateId |> Map.add event.StateId event.StateName }
                transformModel state renameSt
            | InitialStateDesignated event ->
                let initialSt s:WorkflowModel = { s with InitialState = Some event.StateId }
                transformModel state initialSt
            | TerminalStateDesignated event ->
                let termSt s:WorkflowModel = { s with TerminalStates = s.TerminalStates |> Set.add event.StateId }
                transformModel state termSt
            | TerminalStateUnDesignated event ->
                let unTermSt s:WorkflowModel = { s with TerminalStates = s.TerminalStates |> Set.remove event.StateId }
                transformModel state unTermSt
            | TransitionAdded event ->
                let addTr s = 
                    let newTran = {
                        Id = event.TransitionId
                        Name = event.TransitionName
                        InitialState = event.InitialState
                        TargetState = event.TargetState }
                    { s with Transitions = s.Transitions |> Map.add event.TransitionId newTran }
                transformModel state addTr
            | TransitionChanged event ->
                let changeTr s = 
                    let newTran = {
                        Id = event.TransitionId
                        Name = event.TransitionName
                        InitialState = event.InitialState
                        TargetState = event.TargetState }
                    { s with Transitions = s.Transitions |> Map.remove event.TransitionId |> Map.add event.TransitionId newTran }
                transformModel state changeTr
            | TransitionDropped event ->
                let dropTr s = { s with Transitions = s.Transitions |> Map.remove event.TransitionId }
                transformModel state dropTr
            