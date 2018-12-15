namespace FlowMaster.Domain

/// Command Interface Module
///
/// ## Command Interface Module
///
///     let h = Library.hello 1
///     printfn "%d" h
///
module Journey =

    type JourneyId = JourneyId of System.Guid


    /// Returns 42
    ///
    /// ## Parameters
    ///  - `num` - whatever
    type Event =
    | JourneyCreated of JourneyCreationEvent
    | Transitioned of TransitionEvent  
    | Terminated of JourneyEvent
    and JourneyCreationEvent = { JourneyId: JourneyId; WorkflowId: Workflow.WorkflowId; WorkflowVersion: uint64 }
    and JourneyEvent = JourneyId
    and TransitionEvent = { JourneyId: JourneyId; Transition: Workflow.TransitionId }


    type Command =
    | CreateJourney of CreateJourneyCommand
    | Transition of TransitionCommand
    and CreateJourneyCommand = { JourneyId: JourneyId; WorkflowId: Workflow.WorkflowId; WorkflowVersion: uint64; Subject: System.Guid }
    and TransitionCommand = { JourneyId: JourneyId; Transition: Workflow.TransitionId }

    let WorkflowId = function
        | CreateJourney c -> c.JourneyId
        | Transition c -> c.JourneyId

    type Journey = 
    | Active of JourneyModel
    | Terminated of JourneyModel

    and JourneyModel = {
        JourneyId: JourneyId
        WorkflowId: Workflow.WorkflowId
        WorkflowVersion: uint64
        Subject: System.Guid
        CurrentState: Workflow.StateId
        ActiveTransitions: Workflow.TransitionId list
        Name: string }
