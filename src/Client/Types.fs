module App.Types

open Global

type Msg =
    | ListWFMsg of ListWorkflows.Types.Msg
    | WorkflowMsg of ListWorkflows.Types.Msg

type Model =
    { CurrentPage: Page
      ListWorkflows: ListWorkflows.Types.Model }
