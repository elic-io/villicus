namespace Villicus

open Chiron
open Chiron.Operators

module Domain =
    open Villicus.Domain

    type Transition with
        static member ToJson (x: Transition) =
            Json.write "id" x.Id
            *> Json.write "name" x.Name
            *> Json.write "sourceState" x.SourceState
            *> Json.write "targetState" x.TargetState
        static member FromJson (_:Transition) =
              (fun i n s t ->
                { Id = i
                  Name = n
                  SourceState = s
                  TargetState = t })
            <!> Json.read "id"
            <*> Json.read "name"
            <*> Json.read "sourceState"
            <*> Json.read "targetState"

    type State with
        static member ToJson (x:State) =
            Json.write "id" x.Id
            *> Json.write "name" x.Name
            *> Json.write "away" x.Away
            *> Json.write "to" x.To
            *> Json.write "isTerminal" x.IsTerminal
        static member FromJson (_:State) =
              (fun i n a t it ->
                { Id = i
                  Name = n
                  Away = a
                  To = t
                  IsTerminal = it })
            <!> Json.read "id"
            <*> Json.read "name"
            <*> Json.read "away"
            <*> Json.read "to"
            <*> Json.read "isTerminal"

    type WorkflowId with
        static member ToJson (WorkflowId w) = w |> string |> Json.Optic.set Json.String_
        static member FromJson (_:WorkflowId) = 
                fun s ->
                    match System.Guid.TryParse s with
                    | true, x -> x |> WorkflowId |> Json.init
                    | _ -> s |> sprintf "'%s' is not a valid uuid" |> Json.error
            =<< Json.Optic.get Json.String_

    let workflowIdToJson (WorkflowId i) = i |> string |> String

    type Version with
        static member ToJson (Version v) = Json.Optic.set Json.Number_ (decimal v)
        static member FromJson (_:Version) = (uint64 >> Version) <!> Json.Optic.get Json.Number_

    let versionToJson (Version v) = v |> decimal |> Number

    type VersionedWorkflowId with
        static member ToJson (x:VersionedWorkflowId) = 
            Json.write "id" x.Id
            *> Json.writeWith versionToJson "version" x.Version
        static member FromJson (_:VersionedWorkflowId) = json {
            let! i = Json.read "id"
            let! v = Json.read "version"
            return { Id = WorkflowId i; Version = Version v } }
        
    type WorkflowEvent with
        static member ToJson (x:WorkflowEvent) =
            //VersionedWorkflowId.ToJson
            let namedEventToJson eventTypeName (n:WorkflowNamedEvent) = json {
                do! Json.write "eventType" eventTypeName
                do! n.WorkflowId |> fun (WorkflowId i) -> i |> Json.write "workflowId"
                do! Json.write "name" n.Name }
            match x with
            | WorkflowCreated n -> n |> namedEventToJson "workflowCreated"
            | WorkflowRenamed n -> n |> namedEventToJson "workflowRenamed"
            | WorkflowNamed n -> n |> namedEventToJson "workflowNamed"
            | WorkflowCreatedAsCopy cc ->
                Json.write "eventType" "WorkflowCreatedAsCopy"
                *> Json.write "workflowId" cc.WorkflowId
                *> Json.write "source" cc.Source
                *> Json.write "copyName" cc.CopyName
            | WorkflowCopied wc ->
                Json.write "workflowId" wc.WorkflowId 
                *> Json.write "version" wc.Version
                *> Json.write "target" wc.Target
