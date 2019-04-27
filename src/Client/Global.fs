module Global

type Page =
    | About
    | ListWorkflows
    | WorkflowPage of Villicus.Domain.WorkflowId

let toHash page =
    match page with
    | About -> "#about"
    | ListWorkflows -> "#workflows"
    | WorkflowPage (Villicus.Domain.WorkflowId wfid) -> wfid.ToString() |> sprintf "#workflow-%s"
 