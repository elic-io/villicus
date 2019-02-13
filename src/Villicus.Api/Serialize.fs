﻿namespace Villicus

module Serialization =
    open Villicus.Domain
    #if FABLE_COMPILER
    open Thoth.Json
    #else
    open Thoth.Json.Net
    #endif

    type Transition with
        static member Encoder (x:Transition) = Encode.Auto.toString (4, x,true)
        static member Decoder = Decode.Auto.generateDecoder<Transition>(true)

    type State with
        static member Encoder (x:State) = Encode.Auto.toString (4, x,true)
        static member Decoder = Decode.Auto.generateDecoder<State>(true)

    type WorkflowId with
        static member Encoder (WorkflowId x) = Encode.guid x
        static member Decoder s o = Decode.guid s o |> Result.map WorkflowId

    type Version with
        static member Encoder (Version x) = Encode.uint64 x
        static member Decoder s o = Decode.uint64 s o |> Result.map Version

    type VersionedWorkflowId with
        static member Decoder =
            Decode.map2 (fun id version ->
                    { Id = id
                      Version = version })
                 (Decode.field "workflowId" WorkflowId.Decoder)
                 (Decode.field "version" Version.Decoder)
        static member Encoder (x:VersionedWorkflowId) =
            Encode.object 
                [ "workflowId", WorkflowId.Encoder x.Id
                  "version", Version.Encoder x.Version ]

    type WorkflowEvent with
      static member Decoder =
        let decodeNamed eventType =
          Decode.map2
            (fun w n -> {WorkflowId = w; Name = n } |> eventType)
            (Decode.field "workflowId" WorkflowId.Decoder)
            (Decode.field "name" Decode.string)
        let decodeVersion eventType =
          Decode.map2
            (fun w v -> { Id = w; Version = v } |> eventType)
            (Decode.field "workflowId" WorkflowId.Decoder)
            (Decode.field "version" Version.Decoder)
        let decodeStateEdit eventType =
          Decode.map3
            (fun w s sn -> { WorkflowId=w; StateId=s; StateName=sn } |> eventType)
            (Decode.field "workflowId" WorkflowId.Decoder)
            (Decode.field "stateId" Decode.uint32)
            (Decode.field "stateName" Decode.string)
        let decodeState eventType =
          Decode.map2
            (fun w s -> ({ WorkflowId = w; StateId = s }:StateEvent) |> eventType)
            (Decode.field "workflowId" WorkflowId.Decoder)
            (Decode.field "stateId" Decode.uint32)
        let decodeTransitionEdit eventType =
          Decode.map5
            (fun w t tn ss ts -> { WorkflowId = w; TransitionId = t; TransitionName = tn;
                SourceState = ss; TargetState = ts } |> eventType)
            (Decode.field "workflowId" WorkflowId.Decoder)
            (Decode.field "transitionId" Decode.uint32)
            (Decode.field "transitionName" Decode.string)
            (Decode.field "sourceState" Decode.uint32)
            (Decode.field "targetState" Decode.uint32)
        Decode.field "eventType" Decode.string
        |> Decode.andThen (
            function
            | "workflowCreated" -> decodeNamed WorkflowCreated
            | "workflowRenamed" -> decodeNamed WorkflowRenamed
            | "workflowNamed" ->  decodeNamed WorkflowNamed
            | "workflowCreatedAsCopy" ->
              Decode.map3
                (fun w s c -> { WorkflowId = w; Source = s; CopyName = c } |> WorkflowCreatedAsCopy )
                (Decode.field "workflowId" WorkflowId.Decoder)
                (Decode.field "source" VersionedWorkflowId.Decoder)
                (Decode.field "copyName" Decode.string)
            | "workflowCopied" ->
              Decode.map3
                (fun w v t -> { WorkflowId = w; Version = v; Target = t } |> WorkflowCopied )
                (Decode.field "workflowId" WorkflowId.Decoder)
                (Decode.field "version" Version.Decoder)
                (Decode.field "target" WorkflowId.Decoder)
            | "workflowPublished" -> decodeVersion WorkflowPublished
            | "versionIncremented" -> decodeVersion VersionIncremented
            | "workflowWithdrawn" -> decodeVersion WorkflowWithdrawn
            | "stateAdded" -> decodeStateEdit StateAdded
            | "stateRenamed" -> decodeStateEdit StateRenamed
            | "stateDropped" -> decodeState StateDropped
            | "terminalStateDesignated" -> decodeState TerminalStateDesignated
            | "terminalStateUnDesignated" -> decodeState TerminalStateUnDesignated
            | "transitionAdded" -> decodeTransitionEdit TransitionAdded
            | "transitionChanged" -> decodeTransitionEdit TransitionChanged
            | "transitionDropped" ->
              Decode.map2
                (fun w t -> TransitionDropped ({ WorkflowId = w; TransitionId = t }:TransitionDroppedEvent))
                (Decode.field "workflowId" WorkflowId.Decoder)
                (Decode.field "transitionId" Decode.uint32)
            | s -> s |> sprintf "Invalid event type: '%s'" |> Decode.fail)
 
      static member Encoder (x:WorkflowEvent) =
        let eventType,attributes =
          let workflowNamed t (e:WorkflowNamedEvent) = 
            t,
            [ "workFlowId", WorkflowId.Encoder e.WorkflowId
              "name", Encode.string e.Name ]
          let version t (e:VersionedWorkflowId) =
            t,
            [ "workFlowId", WorkflowId.Encoder e.Id
              "version", Version.Encoder e.Version ]
          let stateEdited t (e:StateEditEvent) =
            t,
            [ "workFlowId", WorkflowId.Encoder e.WorkflowId
              "stateId", Encode.uint32 e.StateId
              "stateName", Encode.string e.StateName ]
          let state t (e:StateEvent) =
            t,
            [ "workFlowId", WorkflowId.Encoder e.WorkflowId
              "stateId", Encode.uint32 e.StateId ]
          let transitionEdit t (e:TransitionEditEvent) =
            t,
            [ "workFlowId", WorkflowId.Encoder e.WorkflowId
              "transitionId", Encode.uint32 e.TransitionId
              "transitionName", Encode.string e.TransitionName
              "sourceState", Encode.uint32 e.SourceState
              "targetState", Encode.uint32 e.TargetState ]
          match x with
          | WorkflowCreated e -> workflowNamed "workflowCreated" e
          | WorkflowRenamed e -> workflowNamed "workflowRenamed" e
          | WorkflowNamed e ->  workflowNamed "workflowNamed" e
          | WorkflowCreatedAsCopy e ->
            "workflowCreatedAsCopy",
            [ "workFlowId", WorkflowId.Encoder e.WorkflowId
              "source", VersionedWorkflowId.Encoder e.Source
              "copyName", Encode.string e.CopyName ]
          | WorkflowCopied e -> 
            "workflowCopied",
            [ "workFlowId", WorkflowId.Encoder e.WorkflowId
              "version", Version.Encoder e.Version
              "target", WorkflowId.Encoder e.Target ]
          | WorkflowPublished e -> version "workflowPublished" e
          | VersionIncremented e -> version "versionIncremented" e
          | WorkflowWithdrawn e -> version "workflowWithdrawn" e
          | StateAdded e -> stateEdited "stateAdded" e
          | StateRenamed e -> stateEdited "stateRenamed" e
          | StateDropped e -> state "stateDropped" e
          | TerminalStateDesignated e -> state "terminalStateDesignated" e
          | TerminalStateUnDesignated e -> state "terminalStateUnDesignated" e
          | TransitionAdded e -> transitionEdit "transitionAdded" e
          | TransitionChanged e -> transitionEdit "transitionChanged" e
          | TransitionDropped e ->
            "transitionDropped",
            [ "workFlowId", WorkflowId.Encoder e.WorkflowId
              "transitionId", Encode.uint32 e.TransitionId ]
        [ "eventType", Encode.string eventType ]
        |> List.append attributes
        |> Encode.object

    type WorkflowCommand with
      static member Decoder : Decode.Decoder<WorkflowCommand> =
        let decodeCreate commandType =
          Decode.map2 
            (fun workflowId name -> CreateWorkflowCommand(workflowId,name) |> commandType)
            (Decode.field "workflowId" WorkflowId.Decoder)
            (Decode.field "name" Decode.string)
        let decodePublish commandType =
          Decode.map2
            (fun id version -> commandType { Id = id; Version = version })
            (Decode.field "workflowId" WorkflowId.Decoder)
            (Decode.field "version" Version.Decoder)
        let decodeState commandType =
          Decode.map2 
            (fun workflowId stateId -> commandType { WorkflowId = workflowId; StateId = stateId })
            (Decode.field "workflowId" WorkflowId.Decoder)
            (Decode.field "stateId" Decode.uint32)
        Decode.field "commandType" Decode.string
        |> Decode.andThen (
            function
            | "createWorkflow" -> decodeCreate CreateWorkflow
            | "renameWorkflow" -> decodeCreate RenameWorkflow
            | "copyWorkflow" ->
              Decode.map3
                (fun source target copyName -> 
                    CopyWorkflowCommand(source,target,copyName) |> CopyWorkflow)
                (Decode.field "source" VersionedWorkflowId.Decoder)
                (Decode.field "target" WorkflowId.Decoder)
                (Decode.field "copyName" Decode.string)
            | "publishWorkflow" -> Decode.field "workflowId" WorkflowId.Decoder |> Decode.map PublishWorkflow
            | "rePublishWorkflow" -> decodePublish RePublishWorkflow
            | "withdrawWorkflow" -> decodePublish WithdrawWorkflow
            | "addState" ->
              Decode.map2
                (fun workflowId stateName -> AddStateCommand(workflowId,stateName)|> AddState)
                (Decode.field "workflowId" WorkflowId.Decoder)
                (Decode.field "stateName" Decode.string)
            | "renameState" ->
              Decode.map3
                (fun workflowId stateId stateName -> EditStateCommand(workflowId,stateId,stateName) |> RenameState)
                (Decode.field "workflowId" WorkflowId.Decoder)
                (Decode.field "stateId" Decode.uint32)
                (Decode.field "stateName" Decode.string)
            | "dropState" -> decodeState DropState
            | "setTerminalState" -> decodeState SetTerminalState
            | "unSetTerminalState" -> decodeState UnSetTerminalState
            | "addTransition" ->
              Decode.map4
                (fun w tn ss ts -> AddTransitionCommand(w,tn,ss,ts) |> AddTransition)
                (Decode.field "workflowId" WorkflowId.Decoder)
                (Decode.field "transitionName" Decode.string)
                (Decode.field "sourceState" Decode.uint32)
                (Decode.field "targetState" Decode.uint32)
            | "editTransition" ->
              Decode.map5
                (fun w t tn ss ts -> EditTransitionCommand(w,t,tn,ss,ts) |> EditTransition)
                (Decode.field "workflowId" WorkflowId.Decoder)
                (Decode.field "transitionId" Decode.uint32)
                (Decode.field "transitionName" Decode.string)
                (Decode.field "sourceState" Decode.uint32)
                (Decode.field "targetState" Decode.uint32)
            | "dropTransition" ->
              Decode.map2
                (fun w t -> DropTransition { WorkflowId = w; TransitionId = t })
                (Decode.field "workflowId" WorkflowId.Decoder)
                (Decode.field "transitionId" Decode.uint32)
            | s -> 
                s |> sprintf "Invalid command type: '%s'" |> Decode.fail)

      static member Encoder (x:WorkflowCommand) =
        let commandType,attributes =
          let create t (c:CreateWorkflowCommand) =
            t,
            [ "workFlowId", WorkflowId.Encoder c.WorkflowId
              "name", Encode.string c.Name ]
          let state t (c:StateCommand) =
            t,
            [ "workFlowId", WorkflowId.Encoder c.WorkflowId
              "stateId", Encode.uint32 c.StateId ]
          match x with
          | CreateWorkflow c -> create "createWorkflow" c
          | RenameWorkflow c -> create "renameWorkflow" c
          | CopyWorkflow c ->
            "copyWorkflow",
            [ "source", VersionedWorkflowId.Encoder c.Source
              "target", WorkflowId.Encoder c.Target
              "copyName", Encode.string c.CopyName ]
          | PublishWorkflow c ->
            "publishWorkflow",
            [ "workFlowId", WorkflowId.Encoder c ]
          | RePublishWorkflow c -> 
            "rePublishWorkflow",
            [ "workFlowId", WorkflowId.Encoder c.Id
              "version", Version.Encoder c.Version ]
          | WithdrawWorkflow c ->
            "withdrawWorkflow",
            [ "workFlowId", WorkflowId.Encoder c.Id
              "version", Version.Encoder c.Version ]
          | AddState c ->
            "withdrawWorkflow",
            [ "workFlowId", WorkflowId.Encoder c.WorkflowId
              "stateName", Encode.string c.StateName ]
          | RenameState c ->
            "renameState",
            [ "workFlowId", WorkflowId.Encoder c.WorkflowId
              "stateId", Encode.uint32 c.StateId
              "stateName", Encode.string c.StateName ]
          | DropState c -> state "dropState" c
          | SetTerminalState c -> state "setTerminalState" c
          | UnSetTerminalState c -> state "unSetTerminalState" c
          | AddTransition c ->
            "addTransition",
            [ "workFlowId", WorkflowId.Encoder c.WorkflowId
              "transitionName", Encode.string c.TransitionName
              "sourceState", Encode.uint32 c.SourceState
              "targetState", Encode.uint32 c.TargetState ]
          | EditTransition c ->
            "editTransition",
            [ "workFlowId", WorkflowId.Encoder c.WorkflowId
              "transitionId", Encode.uint32 c.TransitionId
              "transitionName", Encode.string c.TransitionName
              "sourceState", Encode.uint32 c.SourceState
              "targetState", Encode.uint32 c.TargetState ]
          | DropTransition c ->
            "dropTransition",
            [ "workFlowId", WorkflowId.Encoder c.WorkflowId
              "transitionId", Encode.uint32 c.TransitionId ]
        [ "commandType", Encode.string commandType ]
        |> List.append attributes
        |> Encode.object

    type Problems with
        static member Decoder =
            Decode.map3 (fun noTerms unReachable cantReachTerm ->
                { NoTerminalStates = noTerms
                  UnreachableStates = Set.ofArray unReachable
                  CannotReachAnyTerminalState = Set.ofArray cantReachTerm })
              (Decode.field "noTerminalStates" Decode.bool)
              (Decode.field "unreachableStates" (Decode.array Decode.uint32))
              (Decode.field "cannotReachAnyTerminalState" (Decode.array Decode.uint32))
        static member Encoder (x:Problems) =
            Encode.object 
                [ "noTerminalStates", Encode.bool x.NoTerminalStates
                  "unreachableStates", x.UnreachableStates |> Set.toArray |> Array.map Encode.uint32 |> Encode.array
                  "cannotReachAnyTerminalState", x.CannotReachAnyTerminalState |> Set.toArray |> Array.map Encode.uint32 |> Encode.array ]

    let internal mapKey<'value> = Map.toSeq >> Seq.map (fun (k:string,v:'value) -> (uint32 k,v)) >> Map.ofSeq
    let inline internal revMapKey< ^key, ^value when ^key:comparison> = Map.toSeq >> Seq.map (fun (k: ^key,v: ^value) -> ((string k),v)) >> Map.ofSeq
      
    type WorkflowModel with
        static member Decoder =
          Decode.object
            (fun get ->
              let getSet t decode =
                get.Required.Field t (Decode.array decode)
                |> Set.ofArray
              { WorkflowId = get.Required.Field "workflowId" WorkflowId.Decoder
                Name = get.Required.Field "name" Decode.string
                Version = get.Required.Field "version" Version.Decoder
                States = get.Required.Field "states" (Decode.dict State.Decoder) |> mapKey<State>
                InitialStateId = get.Required.Field "initialStateId" Decode.uint32
                TerminalStates = get.Required.Field "terminalStates" (Decode.array Decode.uint32) |> Set.ofArray
                Transitions = get.Required.Field "transitions" (Decode.dict Transition.Decoder) |> mapKey<Transition>
                PublishedVersions = getSet "publishedVersions" Version.Decoder
                Versions = getSet "versions" Version.Decoder
                DirectDescendents = getSet "DirectDescendents" WorkflowId.Decoder
                Ancestors = get.Required.Field "ancestors" (Decode.array VersionedWorkflowId.Decoder) |> Seq.ofArray })
        static member Encoder (x:WorkflowModel) =
          let setToArray encoder = Set.toArray >> Array.map encoder >> Encode.array
          Encode.object 
            [ "workflowId", WorkflowId.Encoder x.WorkflowId
              "name", Encode.string x.Name
              "version", Version.Encoder x.Version
              ("states", x.States |> Map.map (fun _ s ->
                 s |> State.Encoder |> Newtonsoft.Json.Linq.JToken.Parse) 
               |> Map.toSeq |> Seq.map (fun (k,v) -> ((string k),v)) |> Map.ofSeq |> Encode.dict)
              "initialStateId", Encode.uint32 x.InitialStateId
              ("terminalStates", x.TerminalStates |> Set.toArray
               |> Array.map Encode.uint32 |> Encode.array)
              ("transitions", x.Transitions |> Map.map (fun _ t ->
                 t |> Transition.Encoder |> Newtonsoft.Json.Linq.JToken.Parse) 
               |> Map.toSeq |> Seq.map (fun (k,v) -> ((string k),v)) |> Map.ofSeq |> Encode.dict)
              "publishedVersions", x.PublishedVersions |> setToArray Version.Encoder
              "versions", x.Versions |> setToArray Version.Encoder
              "directDescendents", x.DirectDescendents |> setToArray WorkflowId.Encoder
              "ancestors", x.Ancestors |> Seq.toArray |> Array.map VersionedWorkflowId.Encoder |> Encode.array ]

    type MaxCountExceededException with
        static member Encoder (x:MaxCountExceededException) = Encode.Auto.toString (4, x,true)
    type DuplicateWorkflowIdException with
        static member Encoder (x:DuplicateWorkflowIdException) = Encode.Auto.toString (4, x,true)
    type NonExistantWorkflowException with
        static member Encoder (x:NonExistantWorkflowException) = Encode.Auto.toString (4, x,true)
    type UndefinedVersionException with
        static member Encoder (x:UndefinedVersionException) = Encode.Auto.toString (4, x,true)
    type InvalidWorkflowException with
        static member Encoder (x:InvalidWorkflowException) = Encode.Auto.toString (4, x,true)
    type DuplicateStateNameException with
        static member Encoder (x:DuplicateStateNameException) = Encode.Auto.toString (4, x,true)
    type UndefinedStateException with
        static member Encoder (x:UndefinedStateException) = Encode.Auto.toString (4, x,true)
    type InitialStateException with
        static member Encoder (x:InitialStateException) = Encode.Auto.toString (4, x,true)
    type UndefinedTransitionException with
        static member Encoder (x:UndefinedTransitionException) = Encode.Auto.toString (4, x,true)
    type DuplicateTransitionException with
        static member Encoder (x:DuplicateTransitionException) = Encode.Auto.toString (4, x,true)
    type DuplicateTransitionNameException with
        static member Encoder (x:DuplicateTransitionNameException) = Encode.Auto.toString (4, x,true)



    //type WorkflowCommand with
    //    static member Decoder =
    //        Decode.map2 (fun id name ->
    //                { WorkflowId = id
    //                  Name = name })
    //             (Decode.field "workflowId" WorkflowId.Decoder)
    //             (Decode.field "name" Decode.string)
    //    static member Encoder (x:CreateWorkflowCommand) =
    //        Encode.object 
    //            [ "workflowId", WorkflowId.Encoder x.WorkflowId
    //              "name", Encode.string x.Name ]
        //let determineType =
            //    Decode.field "eventType" (Decode.succeed 0)
            //Decode.oneOf 
            //  [ ]