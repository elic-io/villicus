module WorkflowEditor.State

open Elmish
open Types
open Villicus.Domain
open Villicus.Serialization

open Fable.PowerPack
open Fable.Core.JsInterop


let init model : Model * Cmd<Msg> =
  ({ WorkflowId = model.WorkflowId
     Workflow = Some model
     ServerState = Idle
     EditorState = Inactive
     PendingCommands = List.empty }, Cmd.none)

let getJsonSafe (response:Fetch.Fetch_types.Response) = response.text() |> Promise.map (Thoth.Json.Decode.fromString (Thoth.Json.Decode.list WorkflowEvent.Decoder))

let createWorkflow workflowName = CreateWorkflowCommand (System.Guid.NewGuid () |> WorkflowId, workflowName)

//TODO: deal with api paging
let getWorkflow (WorkflowId wfid) : Fable.Import.JS.Promise<List<WorkflowEvent>> = 
  Fetch.fetchAs<List<WorkflowEvent>>
    (wfid.ToString() |> sprintf "/api/workflow/%s")
    (Thoth.Json.Decode.list WorkflowEvent.Decoder)
    []

let sndCommand cmd =
  let json = cmd |> WorkflowCommand.Encoder |> Thoth.Json.Encode.toString 0
  Fetch.postRecord<WorkflowCommand>
    "/api/workflow"
    cmd
    [ Fetch.Fetch_types.RequestProperties.Body !^json ]
  |> Promise.bind getJsonSafe

let update msg model : Model * Cmd<WorkflowId * Msg> =
    match model, msg with
    | _, GetWorkflow ->
        { model with ServerState = Loading }, Cmd.ofPromise getWorkflow model.WorkflowId GotWorkflow ErrorMsg
    | _, GotWorkflow response ->
        { model with
            Workflow = response |> List.fold Workflow.evolve None
            ServerState = Idle }, Cmd.none
    | _, ErrorMsg e -> { model with ServerState = e |> sprintf "%A" |> ServerError  }, Cmd.none
    | _, ActivateEditor -> { model with EditorState = Invalid "" }, Cmd.none
    | _, WorkflowNameChanged s -> 
      let edState =
        match s.Trim(' ') with
        | "" -> Invalid s
        | s' -> Valid s
      { model with EditorState = edState }, Cmd.none
    | { EditorState = Valid s }, AddWorkflow -> 
      { model with EditorState = Adding s },  createWorkflow s |> Types.CreateWorkflow |> Cmd.ofMsg
    | _, CancelAdd ->
      { model with EditorState = Inactive }, Cmd.none
    | _ -> model, Cmd.none
    |> fun (m, c) -> (m, c |> Cmd.map (fun c' -> (model.WorkflowId, c')))