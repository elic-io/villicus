module ListWorkflows.View

open Fable.Core
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fulma
open Types

module KeyCode =
    let enter = 13.
    let upArrow = 38.
    let downArrow =  40.

let onKeyDown keyCode action =
    OnKeyDown (fun (ev:Fable.Import.React.KeyboardEvent) ->
        if ev.keyCode = keyCode then
            ev.preventDefault()
            action ev)

let workflowEdit edModel dispatch =
  match edModel with
    | Inactive ->
      span
        [ ]
        [ button
            [ OnClick (fun _ -> dispatch ActivateEditor) ]
            [ str "Add Workflow" ]
          button
            [ OnClick (fun _ -> dispatch GetWorkflows) ]
            [ str "Refresh" ] ]
    | Invalid s
    | Adding s
    | Valid s ->
      div
        [ Style [ Display "flex" ] ]
        [ span
            [ Style [ FlexGrow 1 ] ]
            [ Input.text
                [ Input.Placeholder "Ex: My Workflow"
                  Input.Value s
                  Input.Props [ OnChange (fun ev -> dispatch (WorkflowNameChanged !!ev.target?value)); onKeyDown KeyCode.enter (fun _ -> dispatch AddWorkflow) ] ] ]
          button
            [ OnClick (fun _ -> AddWorkflow |> dispatch) ]
            [ str "Create" ]
          button
            [ OnClick (fun _ -> CancelAdd |> dispatch) ]
            [ str "Cancel" ] ]

let serverMsg = function
  | ServerError msg -> msg
  | _ -> ""

let root model dispatch =

  div
    [ ]
    [ div
        [ Style [ Float "right" ] ]
        [ span [ Style [ Float "left"; Color "red" ] ] [ model.ServerState |> serverMsg |> str ]
          workflowEdit model.EditorState dispatch ]
      ul
        [ ]
        [ for (_, wfm) in Map.toSeq model.Workflows -> li [] [ wfm |> Option.map(fun w -> w.Name) |> Option.defaultValue "uninitialized" |> str ] ] ]
