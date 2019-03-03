module ListWorkflows.Types

open Villicus.Domain

type ServerState = Idle | Loading | ServerError of string

type EditorState = Invalid of string | Valid of string | Inactive | Adding of string

type Model = {
    Workflows: Map<WorkflowId,Workflow>
    ServerState: ServerState
    EditorState: EditorState
    PendingCommands: Map<WorkflowId, WorkflowCommand * Workflow * List<WorkflowEvent>> }

type Msg =
    | GetWorkflows
    | GotWorkflows of WorkflowModel list
    | ErrorMsg of exn
    | ActivateEditor
    | WorkflowNameChanged of string
    | AddWorkflow
    | CancelAdd
    | CreateWorkflow of CreateWorkflowCommand
    | CreateWorkflowServer of CreateWorkflowCommand
    | CreatedWorkflow of Result<WorkflowEvent list,string>
