module App.Types

open Global

type Msg =
    | ListWFMsg of ListWorkflows.Types.Msg
    | WorkflowMsg of WorkflowEditor.Types.Msg

type Model =
    { CurrentPage: Page
      ListWorkflows: ListWorkflows.Types.Model
      //OpenWorkflows: Map<Villicus.Domain.WorkflowId,WorkflowEditor.Types.Model> }
      ActiveWorkflow: WorkflowEditor.Types.Model option }