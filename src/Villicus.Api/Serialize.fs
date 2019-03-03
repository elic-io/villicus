namespace Villicus
open System.Runtime.InteropServices.ComTypes

module Serialization =
    open Villicus.Domain
    #if FABLE_COMPILER
    open Thoth.Json
    #else
    open Thoth.Json.Net
    #endif

    type Transition with
        static member Decoder = 
          Decode.map4 
            (fun ti n ss ts ->
              { Id = ti
                Name = n
                SourceState = ss
                TargetState = ts } ) 
            (Decode.field "id" Decode.uint64)
            (Decode.field "name" Decode.string)
            (Decode.field "sourceState" Decode.uint64)
            (Decode.field "targetState" Decode.uint64)
           
        static member Encoder (x:Transition) = 
          Encode.object
            [ ("id", Encode.uint64 x.Id)
              ("name", Encode.string x.Name)
              ("sourceState", Encode.uint64 x.SourceState)
              ("targetState", Encode.uint64 x.TargetState) ]

    type State with
        static member Decoder = 
          Decode.map5
            (fun iD n a t isTerm ->
              { Away = Set.ofArray a
                Id = iD
                IsTerminal = isTerm
                Name = n
                To = Set.ofArray t } )
            (Decode.field "id" Decode.uint64)
            (Decode.field "name" Decode.string)
            (Decode.field "away" (Decode.array Decode.uint64))
            (Decode.field "to" (Decode.array Decode.uint64))
            (Decode.field "isTerminal" Decode.bool)

        static member Encoder (x:State) =
          Encode.object
            [ ("id", Encode.uint64 x.Id)
              ("name", Encode.string x.Name)
              ("away", x.Away |> Set.toArray |> Array.map Encode.uint64 |> Encode.array)
              ("to", x.To |> Set.toArray |> Array.map Encode.uint64 |> Encode.array)
              ("isTerminal", Encode.bool x.IsTerminal) ]

    type WorkflowId with
        static member Decoder s o = Decode.guid s o |> Result.map WorkflowId

        static member Encoder (WorkflowId x) = Encode.guid x

    type Version with
        static member Decoder s o = Decode.uint64 s o |> Result.map Version

        static member Encoder (Version x) = Encode.uint64 x

    type VersionedWorkflowId with
        static member Decoder =
            Decode.map2 (fun id version ->
                    { Id = id
                      Version = version })
                 (Decode.field "workflowId" WorkflowId.Decoder)
                 (Decode.field "version" Version.Decoder)

        static member Encoder (x:VersionedWorkflowId) =
            Encode.object 
              [ ("workflowId", WorkflowId.Encoder x.Id)
                ("version", Version.Encoder x.Version) ]

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
            (Decode.field "stateId" Decode.uint64)
            (Decode.field "stateName" Decode.string)
        let decodeState eventType =
          Decode.map2
            (fun w s -> ({ WorkflowId = w; StateId = s }:StateEvent) |> eventType)
            (Decode.field "workflowId" WorkflowId.Decoder)
            (Decode.field "stateId" Decode.uint64)
        let decodeTransitionEdit eventType =
          Decode.map5
            (fun w t tn ss ts -> { WorkflowId = w; TransitionId = t; TransitionName = tn;
                SourceState = ss; TargetState = ts } |> eventType)
            (Decode.field "workflowId" WorkflowId.Decoder)
            (Decode.field "transitionId" Decode.uint64)
            (Decode.field "transitionName" Decode.string)
            (Decode.field "sourceState" Decode.uint64)
            (Decode.field "targetState" Decode.uint64)
        Decode.field "eventType" Decode.string
        |> Decode.andThen (fun x ->
            match x with
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
                  (Decode.field "transitionId" Decode.uint64)
            | s -> s |> sprintf "Invalid event type: '%s'" |> Decode.fail)
 
      static member Encoder (x:WorkflowEvent) =
        let eventType,attributes =
          let workflowNamed t (e:WorkflowNamedEvent) = 
            (t, [ ("workflowId", WorkflowId.Encoder e.WorkflowId); ("name", Encode.string e.Name) ])
          let version t (e:VersionedWorkflowId) =
            (t, [ ("workflowId", WorkflowId.Encoder e.Id); ("version", Version.Encoder e.Version) ])
          let stateEdited t (e:StateEditEvent) =
            (t, [("workflowId", WorkflowId.Encoder e.WorkflowId); ("stateId", Encode.uint64 e.StateId); ("stateName", Encode.string e.StateName) ])
          let state t (e:StateEvent) =
            (t, [ ("workflowId", WorkflowId.Encoder e.WorkflowId); ("stateId", Encode.uint64 e.StateId) ])
          let transitionEdit t (e:TransitionEditEvent) =
            [ ("workflowId", WorkflowId.Encoder e.WorkflowId)
              ("transitionId", Encode.uint64 e.TransitionId)
              ("transitionName", Encode.string e.TransitionName)
              ("sourceState", Encode.uint64 e.SourceState)
              ("targetState", Encode.uint64 e.TargetState) ]
            |> fun a -> (t, a)
          match x with
          | WorkflowCreated e -> workflowNamed "workflowCreated" e
          | WorkflowRenamed e -> workflowNamed "workflowRenamed" e
          | WorkflowNamed e ->  workflowNamed "workflowNamed" e
          | WorkflowCreatedAsCopy e ->
              [ ("workflowId", WorkflowId.Encoder e.WorkflowId)
                ("source", VersionedWorkflowId.Encoder e.Source)
                ("copyName", Encode.string e.CopyName) ]
              |> fun a -> ("workflowCreatedAsCopy", a)
          | WorkflowCopied e -> 
              [ ("workflowId", WorkflowId.Encoder e.WorkflowId)
                ("version", Version.Encoder e.Version)
                ("target", WorkflowId.Encoder e.Target) ]
              |> fun a -> ("workflowCopied", a)
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
              [ ("workflowId", WorkflowId.Encoder e.WorkflowId)
                ("transitionId", Encode.uint64 e.TransitionId) ]
              |> fun a -> ("transitionDropped", a)
        [ ("eventType", Encode.string eventType) ]
        |> List.append attributes
        |> Encode.object

    type WorkflowCommand with
      static member Decoder : Decode.Decoder<WorkflowCommand> =
        let decodeCreate commandType =
          Decode.map2 
            (fun workflowId name -> CreateWorkflowCommand(workflowId, name) |> commandType)
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
            (Decode.field "stateId" Decode.uint64)
        Decode.field "commandType" Decode.string
        |> Decode.andThen (fun x' ->
            match x' with
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
                  (fun workflowId stateName -> AddStateCommand(workflowId, stateName)|> AddState)
                  (Decode.field "workflowId" WorkflowId.Decoder)
                  (Decode.field "stateName" Decode.string)
            | "renameState" ->
                Decode.map3
                  (fun workflowId stateId stateName -> EditStateCommand(workflowId, stateId, stateName) |> RenameState)
                  (Decode.field "workflowId" WorkflowId.Decoder)
                  (Decode.field "stateId" Decode.uint64)
                  (Decode.field "stateName" Decode.string)
            | "dropState" -> decodeState DropState
            | "setTerminalState" -> decodeState SetTerminalState
            | "unSetTerminalState" -> decodeState UnSetTerminalState
            | "addTransition" ->
                Decode.map4
                  (fun w tn ss ts -> AddTransitionCommand(w, tn, ss, ts) |> AddTransition)
                  (Decode.field "workflowId" WorkflowId.Decoder)
                  (Decode.field "transitionName" Decode.string)
                  (Decode.field "sourceState" Decode.uint64)
                  (Decode.field "targetState" Decode.uint64)
            | "editTransition" ->
                Decode.map5
                  (fun w t tn ss ts -> EditTransitionCommand(w, t, tn, ss, ts) |> EditTransition)
                  (Decode.field "workflowId" WorkflowId.Decoder)
                  (Decode.field "transitionId" Decode.uint64)
                  (Decode.field "transitionName" Decode.string)
                  (Decode.field "sourceState" Decode.uint64)
                  (Decode.field "targetState" Decode.uint64)
            | "dropTransition" ->
                Decode.map2
                  (fun w t -> DropTransition { WorkflowId = w; TransitionId = t })
                  (Decode.field "workflowId" WorkflowId.Decoder)
                  (Decode.field "transitionId" Decode.uint64)
            | s -> 
                s |> sprintf "Invalid command type: '%s'" |> Decode.fail)

      static member Encoder (x:WorkflowCommand) =
        let commandType,attributes =
          let create t (c:CreateWorkflowCommand) =
            (t, [ ("workflowId", WorkflowId.Encoder c.WorkflowId); ("name", Encode.string c.Name) ])
          let state t (c:StateCommand) =
            (t, [ ("workflowId", WorkflowId.Encoder c.WorkflowId); ("stateId", Encode.uint64 c.StateId) ])
          match x with
          | CreateWorkflow c -> create "createWorkflow" c
          | RenameWorkflow c -> create "renameWorkflow" c
          | CopyWorkflow c ->
              [ ("source", VersionedWorkflowId.Encoder c.Source)
                ("target", WorkflowId.Encoder c.Target)
                ("copyName", Encode.string c.CopyName) ]
              |> fun a -> ("copyWorkflow", a)
          | PublishWorkflow c ->
              ("publishWorkflow", [ ("workflowId", WorkflowId.Encoder c) ])
          | RePublishWorkflow c -> 
              [ ("workflowId", WorkflowId.Encoder c.Id)
                ("version", Version.Encoder c.Version) ]
              |> fun a -> ("rePublishWorkflow", a)
          | WithdrawWorkflow c ->
              ("withdrawWorkflow", [ ("workflowId", WorkflowId.Encoder c.Id); ("version", Version.Encoder c.Version) ])
          | AddState c ->
              [ ("workflowId", WorkflowId.Encoder c.WorkflowId)
                ("stateName", Encode.string c.StateName) ]
              |> fun a -> ("withdrawWorkflow", a)
          | RenameState c ->
              [ ("workflowId", WorkflowId.Encoder c.WorkflowId)
                ("stateId", Encode.uint64 c.StateId)
                ("stateName", Encode.string c.StateName) ]
              |> fun a -> ("renameState", a)
          | DropState c -> state "dropState" c
          | SetTerminalState c -> state "setTerminalState" c
          | UnSetTerminalState c -> state "unSetTerminalState" c
          | AddTransition c ->
              [ ("workflowId", WorkflowId.Encoder c.WorkflowId)
                ("transitionName", Encode.string c.TransitionName)
                ("sourceState", Encode.uint64 c.SourceState)
                ("targetState", Encode.uint64 c.TargetState) ]
              |> fun a -> ("addTransition", a)
          | EditTransition c ->
              [ ("workflowId", WorkflowId.Encoder c.WorkflowId)
                ("transitionId", Encode.uint64 c.TransitionId)
                ("transitionName", Encode.string c.TransitionName)
                ("sourceState", Encode.uint64 c.SourceState)
                ("targetState", Encode.uint64 c.TargetState) ]
              |> fun a -> ("editTransition", a)
          | DropTransition c ->
              [ ("workflowId", WorkflowId.Encoder c.WorkflowId)
                ("transitionId", Encode.uint64 c.TransitionId) ]
              |> fun a -> ("dropTransition", a)
        [ ("commandType", Encode.string commandType) ]
        |> List.append attributes
        |> Encode.object

    type Problems with
        static member Decoder =
            Decode.map3 (fun noTerms unReachable cantReachTerm ->
                { NoTerminalStates = noTerms
                  UnreachableStates = Set.ofArray unReachable
                  CannotReachAnyTerminalState = Set.ofArray cantReachTerm })
              (Decode.field "noTerminalStates" Decode.bool)
              (Decode.field "unreachableStates" (Decode.array Decode.uint64))
              (Decode.field "cannotReachAnyTerminalState" (Decode.array Decode.uint64))

        static member Encoder (x:Problems) =
            Encode.object
                [ ("noTerminalStates", Encode.bool x.NoTerminalStates)
                  ("unreachableStates", x.UnreachableStates |> Set.toArray |> Array.map Encode.uint64 |> Encode.array)
                  ("cannotReachAnyTerminalState", x.CannotReachAnyTerminalState |> Set.toArray |> Array.map Encode.uint64 |> Encode.array) ]

    let internal mapKey<'value> = Map.toSeq >> Seq.map (fun (k:string,v:'value) -> (uint64 k,v)) >> Map.ofSeq
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
                InitialStateId = get.Required.Field "initialStateId" Decode.uint64
                TerminalStates = get.Required.Field "terminalStates" (Decode.array Decode.uint64) |> Set.ofArray
                Transitions = get.Required.Field "transitions" (Decode.dict Transition.Decoder) |> mapKey<Transition>
                PublishedVersions = getSet "publishedVersions" Version.Decoder
                Versions = getSet "versions" Version.Decoder
                DirectDescendents = getSet "directDescendents" WorkflowId.Decoder
                Ancestors = get.Required.Field "ancestors" (Decode.array VersionedWorkflowId.Decoder) |> Seq.ofArray })

        static member Encoder (x:WorkflowModel) =
          let setToArray encoder = Set.toArray >> Array.map encoder >> Encode.array
          Encode.object 
            [ ("workflowId", WorkflowId.Encoder x.WorkflowId)
              ("name", Encode.string x.Name)
              ("version", Version.Encoder x.Version)
              x.States |> Map.map (fun _ s ->
                 s |> State.Encoder) 
                |> Map.toSeq |> Seq.map (fun (k, v) -> (string k, v)) |> Map.ofSeq |> Encode.dict
              |> fun a -> ("states", a)
              ("initialStateId", Encode.uint64 x.InitialStateId)
              (("terminalStates", x.TerminalStates |> Set.toArray
                |> Array.map Encode.uint64 |> Encode.array))
              x.Transitions |> Map.map (fun _ t ->
                 t |> Transition.Encoder)
                |> Map.toSeq |> Seq.map (fun (k,v) -> ((string k), v)) |> Map.ofSeq |> Encode.dict
              |> fun a -> ("transitions", a)
              ("publishedVersions", x.PublishedVersions |> setToArray Version.Encoder)
              ("versions", x.Versions |> setToArray Version.Encoder)
              ("directDescendents", x.DirectDescendents |> setToArray WorkflowId.Encoder)
              ("ancestors", x.Ancestors |> Seq.toArray |> Array.map VersionedWorkflowId.Encoder |> Encode.array) ]

    type MaxCountExceededException with
        static member Encoder (x:MaxCountExceededException) = Encode.Auto.toString (4, x, true)
    type DuplicateWorkflowIdException with
        static member Encoder (x:DuplicateWorkflowIdException) = Encode.Auto.toString (4, x, true)
    type NonExistantWorkflowException with
        static member Encoder (x:NonExistantWorkflowException) = Encode.Auto.toString (4, x, true)
    type UndefinedVersionException with
        static member Encoder (x:UndefinedVersionException) = Encode.Auto.toString (4, x, true)
    type InvalidWorkflowException with
        static member Encoder (x:InvalidWorkflowException) = Encode.Auto.toString (4, x, true)
    type DuplicateStateNameException with
        static member Encoder (x:DuplicateStateNameException) = Encode.Auto.toString (4, x, true)
    type UndefinedStateException with
        static member Encoder (x:UndefinedStateException) = Encode.Auto.toString (4, x, true)
    type InitialStateException with
        static member Encoder (x:InitialStateException) = Encode.Auto.toString (4, x, true)
    type UndefinedTransitionException with
        static member Encoder (x:UndefinedTransitionException) = Encode.Auto.toString (4, x, true)
    type DuplicateTransitionException with
        static member Encoder (x:DuplicateTransitionException) = Encode.Auto.toString (4, x, true)
    type DuplicateTransitionNameException with
        static member Encoder (x:DuplicateTransitionNameException) = Encode.Auto.toString (4, x, true)



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
