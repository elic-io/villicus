module WorkflowEditor.Types

open Villicus.Domain

type ServerState = Idle | Loading | ServerError of string

type EditorState = Invalid of string | Valid of string | Inactive | Adding of string

type Model = {
    WorkflowId: WorkflowId
    Workflow: Workflow
    ServerState: ServerState
    EditorState: EditorState
    PendingCommands: List<WorkflowCommand * Workflow * List<WorkflowEvent>> }

type Msg =
    | GetWorkflow
    | GotWorkflow of WorkflowEvent list
    | ErrorMsg of exn
    | ActivateEditor
    | WorkflowNameChanged of string
    | AddWorkflow
    | CancelAdd
    | WorkflowMsg of WorkflowCommand
    | WorkflowMsgServer of WorkflowCommand
    | CreateWorkflow of CreateWorkflowCommand
    | CreateWorkflowServer of CreateWorkflowCommand
    | CreatedWorkflow of Result<WorkflowEvent list,string>
