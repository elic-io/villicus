module App.State

open Elmish
open Elmish.Browser.Navigation
open Elmish.Browser.UrlParser
open Fable.Import.Browser
open Global
open Types

let pageParser : Parser<Page->Page,Page> =
    oneOf
      [ map About (s "about")
        map (fun (s:string) -> s |> System.Guid |> Villicus.Domain.WorkflowId |> WorkflowPage) (s "workflow" </> str)
        map ListWorkflows (s "workflows") ]

let urlUpdate (result : Page option) model =
    match result with
    | None ->
        console.error("Error parsing url")
        model, Navigation.modifyUrl (toHash model.CurrentPage)
    | Some page ->
        { model with CurrentPage = page }, []

let init result =
    let (listWorkflows, wflCmd) = ListWorkflows.State.init()
    let (model, cmd) =
        urlUpdate result
          { CurrentPage = ListWorkflows
            ListWorkflows = listWorkflows
            // OpenWorkflows = Map.empty }
            ActiveWorkflow = None }

    model, Cmd.batch [ cmd
                       Cmd.map ListWFMsg wflCmd
                       Cmd.map WorkflowMsg Cmd.none  ]

let update msg model =
    match msg with
    | ListWFMsg msg ->
        match msg with
        | WorkflowEditPage wfModel ->
            let (workflow, wfCmd) = WorkflowEditor.State.init wfModel
            { model with ActiveWorkflow = workflow; CurrentPage = WorkflowPage workflow.WorkflowId }
        | msg ->
            let (listWF, listWFCmd) = ListWorkflows.State.update msg model.ListWorkflows
            { model with ListWorkflows = listWF }, Cmd.map ListWFMsg listWFCmd
    | WorkflowMsg (msg) ->
        let (workflow, WFCmd) =
            // let WF = Map.find workflowId model.OpenWorkflows
            // WorkflowEditor.State.update msg WF
            WorkflowEditor.State.update msg workflow
        { model with OpenWorkflows = Map.add workflowId workflow model.OpenWorkflows }, Cmd.map WorkflowMsg WFCmd