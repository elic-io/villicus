module WorkflowEditor.View

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

let serverMsg = function
  | ServerError msg -> msg
  | _ -> ""

let root model dispatch =

  div
    [ ]
    [ model.Workflow |> Option.map(fun w -> w.Name) |> Option.defaultValue "uninitialized" |> str
      div
        [ Style [ Float "right" ] ]
        [ span [ Style [ Float "left"; Color "red" ] ] [ model.ServerState |> serverMsg |> str ] ] ]
