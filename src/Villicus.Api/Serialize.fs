namespace Villicus

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
        let decodeNamed (get:Decode.IGetters) =
            { WorkflowId = get.Required.Field "workflowId" WorkflowId.Decoder
              Name = get.Required.Field "name" Decode.string }
        let decodeVersion (get:Decode.IGetters) =
            { Id = get.Required.Field "id" WorkflowId.Decoder 
              Version = get.Required.Field "version" Version.Decoder }
        let decodeStateEdit (get:Decode.IGetters) =
            { WorkflowId = get.Required.Field "workflowId" WorkflowId.Decoder 
              StateId = get.Required.Field "stateId" Decode.uint32
              StateName = get.Required.Field "stateName" Decode.string }
        let decodeState (get:Decode.IGetters) =
            { StateEvent.WorkflowId =
                get.Required.Field "workflowId" WorkflowId.Decoder 
              StateEvent.StateId =
                get.Required.Field "stateId" Decode.uint32 }
        let decodeTransitionEdit (get:Decode.IGetters) =
            { WorkflowId = get.Required.Field "workflowId" WorkflowId.Decoder 
              TransitionId = get.Required.Field "transitionId" Decode.uint32
              TransitionName = get.Required.Field "transitionName" Decode.string
              SourceState = get.Required.Field "sourceState" Decode.uint32
              TargetState = get.Required.Field "targetState" Decode.uint32 }
        Decode.object
          (fun get ->
            match get.Required.Field "eventType" Decode.string with
            | "workflowCreated" -> decodeNamed get |> WorkflowCreated |> Decode.succeed
            | "workflowRenamed" -> decodeNamed get |> WorkflowRenamed |> Decode.succeed
            | "workflowNamed" ->  decodeNamed get |> WorkflowNamed |> Decode.succeed
            | "workflowCreatedAsCopy" ->
                { WorkflowId = get.Required.Field "workflowId" WorkflowId.Decoder
                  Source = get.Required.Field "source" VersionedWorkflowId.Decoder
                  CopyName = get.Required.Field "copyName" Decode.string }
                |> WorkflowCreatedAsCopy |> Decode.succeed
            | "workflowCopied" ->
                { WorkflowId = get.Required.Field "workflowId" WorkflowId.Decoder
                  Version = get.Required.Field "version" Version.Decoder
                  Target = get.Required.Field "target" WorkflowId.Decoder }
                |> WorkflowCopied |> Decode.succeed
            | "workflowPublished" -> decodeVersion get |> WorkflowPublished |> Decode.succeed
            | "versionIncremented" -> decodeVersion get |> VersionIncremented |> Decode.succeed
            | "workflowWithdrawn" -> decodeVersion get |> WorkflowWithdrawn |> Decode.succeed
            | "stateAdded" -> decodeStateEdit get |> StateAdded |> Decode.succeed
            | "stateRenamed" -> decodeStateEdit get |> StateRenamed |> Decode.succeed
            | "stateDropped" -> decodeState get |> StateDropped |> Decode.succeed
            | "terminalStateDesignated" -> decodeState get |> TerminalStateDesignated |> Decode.succeed
            | "terminalStateUnDesignated" -> decodeState get |> TerminalStateUnDesignated |> Decode.succeed
            | "transitionAdded" -> decodeTransitionEdit get |> TransitionAdded |> Decode.succeed
            | "transitionChanged" -> decodeTransitionEdit get |> TransitionChanged |> Decode.succeed
            | "transitionDropped" -> 
                { TransitionDroppedEvent.WorkflowId =
                    get.Required.Field "workflowId" WorkflowId.Decoder 
                  TransitionDroppedEvent.TransitionId =
                    get.Required.Field "transitionId" Decode.uint32 }
                |> TransitionDropped |> Decode.succeed
            | s -> s |> sprintf "Invalid event type: '%s'" |> Decode.fail )
 
      static member Encoder (x:WorkflowEvent) =
        let eventType,attributes =
          let workflowNamed t (e:WorkflowNamedEvent) = 
            t,
            [ "workFlowId", WorkflowId.Encoder e.WorkflowId
              "name", Encode.string e.Name ]
          let version t (e:VersionedWorkflowId) =
            t,
            [ "id", WorkflowId.Encoder e.Id
              "version", Version.Encoder e.Version ]
          let stateEdited t (e:StateEditEvent) =
            t,
            [ "id", WorkflowId.Encoder e.WorkflowId
              "stateId", Encode.uint32 e.StateId
              "stateName", Encode.string e.StateName ]
          let state t (e:StateEvent) =
            t,
            [ "id", WorkflowId.Encoder e.WorkflowId
              "stateId", Encode.uint32 e.StateId ]
          let transitionEdit t (e:TransitionEditEvent) =
            t,
            [ "id", WorkflowId.Encoder e.WorkflowId
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
            [ "id", WorkflowId.Encoder e.WorkflowId
              "transitionId", Encode.uint32 e.TransitionId ]
        [ "eventType", Encode.string eventType ]
        |> List.append attributes
        |> Encode.object

    type WorkflowCommand with
      static member Decoder =
        let decodeCreate (get:Decode.IGetters) = 
          CreateWorkflowCommand(
            get.Required.Field "workflowId" WorkflowId.Decoder,
            get.Required.Field "name" Decode.string)
        let state (get:Decode.IGetters) = 
          { WorkflowId = get.Required.Field "workflowId" WorkflowId.Decoder
            StateId = get.Required.Field "stateId" Decode.uint32 }
        Decode.object
          (fun get ->
            match get.Required.Field "commandType" Decode.string with
            | "createWorkflow" -> decodeCreate get |> CreateWorkflow |> Decode.succeed
            | "renameWorkflow" -> decodeCreate get |> RenameWorkflow |> Decode.succeed
            | "copyWorkflow" ->
              CopyWorkflowCommand(
                get.Required.Field "source" VersionedWorkflowId.Decoder,
                get.Required.Field "target" WorkflowId.Decoder,
                get.Required.Field "copyName" Decode.string )
              |> CopyWorkflow |> Decode.succeed
            | "publishWorkflow" ->
              get.Required.Field "workflowId" WorkflowId.Decoder
              |> PublishWorkflow |> Decode.succeed
            | "rePublishWorkflow" ->
              { Id = get.Required.Field "workflowId" WorkflowId.Decoder
                Version = get.Required.Field "version" Version.Decoder }
              |> RePublishWorkflow |> Decode.succeed
            | "withdrawWorkflow" ->
              { Id = get.Required.Field "workflowId" WorkflowId.Decoder
                Version = get.Required.Field "version" Version.Decoder }
              |> WithdrawWorkflow |> Decode.succeed
            | "addState" ->
              AddStateCommand(
                get.Required.Field "workflowId" WorkflowId.Decoder,
                get.Required.Field "stateName" Decode.string )
              |> AddState |> Decode.succeed
            | "renameState" ->
              EditStateCommand(
                get.Required.Field "workflowId" WorkflowId.Decoder,
                get.Required.Field "stateId" Decode.uint32,
                get.Required.Field "stateName" Decode.string )
              |> RenameState |> Decode.succeed
            | "dropState" -> state get |> DropState |> Decode.succeed
            | "setTerminalState" -> state get |> SetTerminalState |> Decode.succeed
            | "unSetTerminalState" -> state get |> UnSetTerminalState |> Decode.succeed
            | "addTransition" -> 
              AddTransitionCommand(
                get.Required.Field "workflowId" WorkflowId.Decoder,
                get.Required.Field "transitionName" Decode.string,
                get.Required.Field "sourceState" Decode.uint32,
                get.Required.Field "targetState" Decode.uint32)
              |> AddTransition |> Decode.succeed
            | "editTransition" ->
              EditTransitionCommand(
                get.Required.Field "workflowId" WorkflowId.Decoder,
                get.Required.Field "transitionId" Decode.uint32,
                get.Required.Field "transitionName" Decode.string,
                get.Required.Field "sourceState" Decode.uint32,
                get.Required.Field "targetState" Decode.uint32)
              |> EditTransition |> Decode.succeed
            | "dropTransition" ->
              { WorkflowId = get.Required.Field "workflowId" WorkflowId.Decoder
                TransitionId = get.Required.Field "transitionId" Decode.uint32 }
              |> DropTransition |> Decode.succeed
            | s -> s |> sprintf "Invalid command type: '%s'" |> Decode.fail )
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
