module ListWorkflows.State

open Elmish
open Types
open Villicus.Domain
open Villicus.Serialization

open Fable.PowerPack
open Fable.Core.JsInterop


let init () : Model * Cmd<Msg> =
  ({ Workflows = Map.empty
     ServerState = Idle
     EditorState = Inactive
     PendingCommands = Map.empty }, Cmd.ofMsg GetWorkflows)

let getJsonSafe (response:Fetch.Fetch_types.Response) = response.text() |> Promise.map (Thoth.Json.Decode.fromString (Thoth.Json.Decode.list WorkflowEvent.Decoder))

let createWorkflow workflowName = CreateWorkflowCommand (System.Guid.NewGuid () |> WorkflowId, workflowName)

let getResponse () = 
  Fetch.fetchAs<List<WorkflowModel>>
    "/api/workflow"
    (Thoth.Json.Decode.list WorkflowModel.Decoder)
    []

let sndCommand cmd =
  let json = cmd |> WorkflowCommand.Encoder |> Thoth.Json.Encode.toString 0
  Fetch.postRecord<WorkflowCommand>
    "/api/workflow"
    cmd
    [ Fetch.Fetch_types.RequestProperties.Body !^json ]
  |> Promise.bind getJsonSafe

let update msg model : Model * Cmd<Msg> =
    match model, msg with
    | _, GetWorkflows ->
        { model with ServerState = Loading }, Cmd.ofPromise getResponse () GotWorkflows ErrorMsg
    | _, GotWorkflows response ->
        { model with
            Workflows = response |> List.map (fun x -> (x.WorkflowId, Some x) ) |> Map.ofList
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
    | _, Types.CreateWorkflow cmd ->
      match Workflow.handle (CreateWorkflow cmd) None with
      | Ok events ->
        match events |> List.fold Workflow.evolve None with
        | Some newWF ->
          { model with
              Workflows = Map.add newWF.WorkflowId (Some newWF) model.Workflows
              EditorState = Inactive
              PendingCommands = Map.add newWF.WorkflowId (CreateWorkflow cmd, None, events) model.PendingCommands
          }, cmd |> Types.CreateWorkflowServer |> Cmd.ofMsg
        | None -> model, Cmd.none        
      | Error e -> { model with ServerState = ServerError e.Message }, Cmd.none
    | _, Types.CreateWorkflowServer cmd ->
      { model with ServerState = Loading }, Cmd.ofPromise sndCommand (CreateWorkflow cmd) CreatedWorkflow ErrorMsg
    | _, CreatedWorkflow response ->
      match response with
      | Ok events ->
        { model with
            ServerState = Idle
            PendingCommands = model.PendingCommands |> Map.remove (events |> List.head |> Workflow.eWorkflowId) }, Cmd.none
      | Error e ->
        { model with
            ServerState = ServerError ("ARGH! " + e) }, Cmd.none
    | _ -> model, Cmd.none