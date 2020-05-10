module WorkflowTests

open System
open Xunit
open Villicus.Domain
open Villicus.Domain.CommandHelpers
open Villicus.Domain.CommandAPI
open TestUtil

let argNull =
    function
    | Error (NullArgument _) -> ()
    | Ok _
    | Error (_) -> raise (exn "expected null Error")

let someWorkflow = Result.bind(Result.ofOption NonExistant)

[<Fact>]
let ``CreateCommand missing name`` () =    
    argNull <| newCreateWorkflowCommand (Guid.NewGuid() |> WorkflowId) ""
    argNull <| newCreateWorkflowCommand (Guid.NewGuid() |> WorkflowId) null
    argNull <| newCreateWorkflowCommand (Guid.NewGuid() |> WorkflowId) " "

[<Fact>]
let ``AddStateCommand missing name`` () =
    argNull <| newAddStateCommand (Guid.NewGuid() |> WorkflowId)  ""

[<Fact>]
let ``EditStateCommand missing name`` () =
    argNull <| newEditStateCommand (Guid.NewGuid() |> WorkflowId) 0u ""

[<Fact>]
let ``AddTransitionCommand missing name`` () =
    argNull <| newAddTransitionCommand (Guid.NewGuid() |> WorkflowId) "" 0u 1u

let argDupe =
    function
    | Error (CantTargetSelf _) -> ()
    | Ok _
    | Error (_) -> raise (exn "expected duplicate states Error")


[<Fact>]
let ``AddTransitionCommand duplicate states`` () =
    argDupe <| newAddTransitionCommand (Guid.NewGuid() |> WorkflowId) "Duplicate States" 0u 0u

[<Fact>]
let ``EditTransitionCommand missing name`` () =
    argNull <| newEditTransitionCommand (Guid.NewGuid() |> WorkflowId) 0u "" 0u 1u

[<Fact>]
let ``EditTransitionCommand duplicate states`` () =
    argDupe <| newEditTransitionCommand (Guid.NewGuid() |> WorkflowId) 0u "Duplicate States" 0u 0u

let handleEvolve command state =
    Workflow.handle command state
    |> Result.map (Seq.fold Workflow.evolve state)

let processCmd command wf =
    command
    |> Result.mapError CommandCreation
    |> (Result.bind (fun cmd -> (Result.bind (handleEvolve cmd)) wf) )

let testCreateWorkflow iD name : Result<Workflow,WorkflowError> =
    createWorkflow iD name
    |> Result.mapError CommandCreation
    |> Result.bind (fun createWfCommand -> handleEvolve createWfCommand None)

let testWorkflowId = Guid.NewGuid() |> WorkflowId
let testWorkFlowName = "TestWorkflow"
let testWorkflow = testCreateWorkflow testWorkflowId testWorkFlowName

[<Fact>]
let ``workflow creation`` () =
    newCreateWorkflowCommand testWorkflowId testWorkFlowName
    |> Result.map2  CreateWorkflow CommandCreation
    |> Result.bind (fun x -> Workflow.handle x None)
    |> Result.map (fun events -> 
        Assert.Equal(4, Seq.length events)
        Seq.fold Workflow.evolve None events)
    |> Result.map(fun wf -> Assert.True(wf.IsSome); wf)
    |> Result.map (Option.map (fun x ->
                Assert.Equal(testWorkFlowName,x.Name)
                Assert.Equal(testWorkflowId,x.WorkflowId)))
    |> raiseIfError

[<Fact>]
let ``duplicate workflow error`` () =
    let err =
        let createCmd =
            createWorkflow testWorkflowId "Test2"
        testWorkflow
        |> (processCmd createCmd)
    Assert.Equal(WorkflowError.duplicate testWorkflowId |> Error, err)

[<Fact>]
let ``simple created workflow is valid`` () =
    testWorkflow
    |> Result.map (Option.bind (fun m -> 
            Workflow.problems m
            |> Option.map (fun problems ->
                WorkflowError.invalid m.WorkflowId problems)))
    |> raiseIfError

[<Fact>]
let ``publish workflow`` () =
    testWorkflow
    |> Result.map(Option.map(fun before ->
        testWorkflow
        |> processCmd (publishWorkflow testWorkflowId)
        |> Result.map(Option.map(fun m ->
            Assert.True(before.Version |> m.PublishedVersions.Contains)
            Assert.Equal(before.Version.Inc,m.Version)
            Assert.Equal(2,m.Versions.Count)))))
    |> raiseIfError

[<Fact>]
let ``republish workflow on an unpublished workflow is the same as publish`` () =
    testWorkflow
    |> Result.map(Option.map(fun before ->
        testWorkflow
        |> processCmd (rePublishWorkflow before.VersionedWorkflowId)
        |> Result.map(Option.map(fun m ->
            Assert.True(before.Version |> m.PublishedVersions.Contains)
            Assert.Equal(before.Version.Inc,m.Version)))))
    |> raiseIfError

[<Fact>]
let ``cannot republish a version that doesn't exist`` () =
    testWorkflow
    |> Result.map(Option.map(fun before ->
        let rePubCmd =
            { Id = before.WorkflowId; Version = before.Version.Inc }
            |> rePublishWorkflow
        testWorkflow
        |> processCmd rePubCmd
        |> Result.map2
            (fun _ -> raise (exn "this should have been an UndefinedVersion error"))
            (fun e -> Assert.True(match e with | UndefinedVersion _ -> true | _ -> false))))
    |> ignore

let dec = (fun (Version i) -> i-1UL |> Version)

[<Fact>]
let ``republish a published version generates empty event set`` () =
    let published =
        testWorkflow
        |> processCmd (publishWorkflow testWorkflowId)
    published |> Result.map(Option.map(fun p -> 
            let pubVersionId = { 
                Id = p.WorkflowId 
                Version = p.PublishedVersions |> Set.toSeq |> Seq.head }
            published
            |> Result.bind (Workflow.handle (RePublishWorkflow pubVersionId))
            |> Result.map(fun events -> Assert.True(Seq.isEmpty events))))
    |> raiseIfError

[<Fact>]
let ``withdraw a version`` () =
    let published =
        testWorkflow
        |> processCmd (publishWorkflow testWorkflowId)
    published |> Result.map(Option.map(fun p -> 
            let pubVersion = p.PublishedVersions |> Set.toSeq |> Seq.head
            let pubVersionId = { Id = p.WorkflowId; Version = pubVersion }
            published
            |> processCmd (withdrawWorkflow pubVersionId)
            |> Result.map(Option.map(fun w -> Assert.False(w.PublishedVersions.Contains pubVersion)))))
    |> raiseIfError

[<Fact>]
let ``republish a withdrawn version`` () =
    let published =
        testWorkflow
        |> processCmd (publishWorkflow testWorkflowId)
    published |> Result.map(Option.map(fun p -> 
            let pubVersion = p.PublishedVersions |> Set.toSeq |> Seq.head
            let pubVersionId = { Id = p.WorkflowId; Version = pubVersion }
            published
            |> processCmd (withdrawWorkflow pubVersionId)
            |> processCmd (rePublishWorkflow pubVersionId)
            |> Result.map(Option.map(fun w -> Assert.True(pubVersion |> w.PublishedVersions.Contains)))))
    |> raiseIfError

[<Fact>]
let ``add state`` () =
    testWorkflow
    |> processCmd (addState testWorkflowId "Orphan")
    |> someWorkflow
    |> Result.map(fun s ->
        Assert.Equal(3,s.States.Count)
        Assert.Equal("Orphan",Map.find 2u s.States |> (fun x -> x.Name)))
    |> raiseIfError

[<Fact>]
let ``add state with same name as existing`` () =
    testWorkflow
    |> processCmd (addState testWorkflowId "Orphan")
    |> processCmd (addState testWorkflowId "Orphan")
    |> someWorkflow
    |> Result.map2
        (fun _ -> raise (exn "this should have been an duplicate workflowId error"))
        (fun e -> Assert.True(match e with | DuplicateStateName _ -> true | _ -> false))
    |> ignore

[<Fact>]
let ``rename state`` () =
    testWorkflow
    |> processCmd (renameState testWorkflowId 0u "Renamed Initial State")
    |> someWorkflow
    |> Result.map(fun s ->
        Assert.Equal(2,s.States.Count)
        Assert.Equal("Renamed Initial State",Map.find 0u s.States |> (fun x -> x.Name)))
    |> raiseIfError

[<Fact>]
let ``rename state that doesn't exist`` () =
    testWorkflow
    |> processCmd (renameState testWorkflowId 2u "Rename NonExistant State")
    |> someWorkflow
    |> Result.map2
        (fun _ -> raise (exn "this should have been an UndefinedState error"))
        (fun e -> Assert.Equal(WorkflowError.undefinedState testWorkflowId 2u, e))
    |> ignore

[<Fact>]
let ``can't publish invalid workflow`` () =
    testWorkflow
    |> processCmd (addState testWorkflowId "Orphan")
    |> processCmd (publishWorkflow testWorkflowId)
    |> Result.map2
        (fun _ -> raise (exn "this should have been an Invalid Workflow error"))
        (fun e -> Assert.True( match e with | Invalid _ -> true | _ -> false))
    |> ignore

[<Fact>]
let ``orphaned state makes workflow invalid`` () =
    testWorkflow
    |> processCmd (addState testWorkflowId "Orphan")
    |> Result.map (fun model ->
        model 
        |> Option.bind (fun m -> 
            Assert.True((Workflow.problems m).IsSome)
            Workflow.problems m
            |> Option.map (fun p -> 
                Assert.False(p.Valid)
                Assert.True(2u |> p.UnreachableStates.Contains))))
    |> raiseIfError

[<Fact>]
let ``state that can't reach terminal makes workflow invalid`` () =
    testWorkflow
    |> processCmd (addState testWorkflowId "Not Terminated")
    |> processCmd (addTransition testWorkflowId "to nonTerminated" 0u 2u)
    |> Result.map(
        Option.bind (fun m -> 
            Assert.True((Workflow.problems m).IsSome)
            Workflow.problems m
            |> Option.map (fun p -> 
                Assert.False(p.Valid)
                Assert.False(p.UnreachableStates.Contains 2u)
                Assert.True(p.CannotReachAnyTerminalState.Contains 2u))))
    |> raiseIfError

[<Fact>]
let ``workflow without terminal states is invalid`` () =
    testWorkflow
    |> processCmd (unSetTerminalState testWorkflowId 1u)
    |> Result.map(
        Option.bind (fun m -> 
            Assert.True((Workflow.problems m).IsSome)
            Workflow.problems m
            |> Option.map (fun p -> 
                Assert.False(p.Valid)
                Assert.False(p.UnreachableStates.Contains 1u)
                Assert.True(p.CannotReachAnyTerminalState.Contains 0u)
                Assert.True(p.CannotReachAnyTerminalState.Contains 1u)
                Assert.True(p.NoTerminalStates))))
    |> raiseIfError

// since this is an extreme edge case, we're probably alright
[<Fact (Skip="Takes too long to run every time")>]
let ``workflow state count exceeded returns correct error`` () =
    let rec processAll i limit a =
        let b = a |> processCmd (addState testWorkflowId (sprintf "State %u" i))
        if i = limit then b else processAll (i+1u) limit b
    testWorkflow
    |> processAll 2u System.UInt32.MaxValue
    |> processCmd (addState testWorkflowId "One State too many")
    |> Result.map2
        (fun _ -> raise (exn "this should have been an max count exceeded error"))
        (fun e -> Assert.True( match e with | MaxCountExceeded _ -> true | _ -> false))
    |> ignore

[<Fact>]
let ``copy Workflow`` () =
    let oldWorkflow =
        match testWorkflow with
        | Ok twf -> match twf with | Some x -> x | None -> failwith "won't happen"
        | Error err -> raise (err |> sprintf "Error: %O" |> exn)
    let copyCmd =
        copyWorkflow
          { Id = testWorkflowId; Version = 1UL |> Version }
          (Guid.NewGuid() |> WorkflowId)
          (oldWorkflow.Name |> sprintf "Copy of %s")
        |> Result.mapError CommandCreation

    let handleResult wf =
        copyCmd
        |> Result.bind (fun c -> Workflow.handle c wf)
    let events = testWorkflow |> Result.bind handleResult
    let origEvents = events |> Result.map(Seq.filter(fun e -> oldWorkflow.WorkflowId = Workflow.eWorkflowId e ))
    let copyEvents = events |> Result.map(Seq.filter(fun e -> oldWorkflow.WorkflowId <> Workflow.eWorkflowId e ))
    testWorkflow |> Result.bind(fun state ->
        origEvents |> Result.map (Seq.fold Workflow.evolve state))
    |> Result.map(fun origWF ->
        copyEvents |> Result.map (Seq.fold Workflow.evolve None)
        |> Result.map(
            origWF |>
                ((fun o n ->
                    Assert.Equal<Map<StateId,State>>((o.States),(n.States))
                    Assert.Equal<Map<TransitionId,Transition>>(o.Transitions,n.Transitions)
                    Assert.Equal<Set<StateId>>(o.TerminalStates,n.TerminalStates)
                    Assert.Contains(o.VersionedWorkflowId,n.Ancestors)
                    Assert.Contains(n.WorkflowId,o.DirectDescendents)
                    Assert.Equal(n.Name,sprintf "Copy of %s" o.Name)
                    ())
                    |> Option.lift2)))
    |> raiseIfError

[<Fact>]
let ``drop state`` () =
    testWorkflow
    |> processCmd (dropState testWorkflowId 1u)
    |> someWorkflow
    |> Result.map(fun s ->
        Assert.Equal(1,s.States.Count))
    |> raiseIfError

[<Fact>]
let ``can't drop initial state`` () =
    testWorkflow
    |> processCmd (dropState testWorkflowId 0u)
    |> someWorkflow
    |> Result.map2
        (fun _ -> raise (exn "this should have been an CantRemoveInitialState Error"))
        (fun e -> Assert.True( match e with | CantRemoveInitialState _ -> true | _ -> false))
    |> ignore

[<Fact>]
let ``can't drop nonexistant state`` () =
    testWorkflow
    |> processCmd (dropState testWorkflowId 2u)
    |> someWorkflow
    |> Result.map2
        (fun _ -> raise (exn "this should have been an UndefinedState Error"))
        (fun e -> Assert.True( match e with | UndefinedState _ -> true | _ -> false))
    |> ignore

[<Fact>]
let ``set terminal state`` () =
    testWorkflow
    |> processCmd (addState testWorkflowId "New Terminal")
    |> processCmd (setTerminalState testWorkflowId 2u)
    |> someWorkflow
    |> Result.map(fun s ->
        let newTerminal = s.States |> Map.find 2u
        Assert.True(s.TerminalStates.Contains 2u)
        Assert.True(newTerminal.IsTerminal))
    |> raiseIfError

[<Fact>]
let ``set terminal state that is already terminal generates no events`` () =
    testWorkflow
    |> processCmd (addState testWorkflowId "New Terminal")
    |> processCmd (setTerminalState testWorkflowId 2u)
    |> Result.bind(Workflow.handle (SetTerminalState { WorkflowId = testWorkflowId; StateId = 2u }))
    |> Result.map(Seq.isEmpty >> Assert.True)
    |> raiseIfError

[<Fact>]
let ``Setting terminal state that doesn't exist`` () =
    testWorkflow
    |> processCmd (setTerminalState testWorkflowId 2u)
    |> Result.map2
        (fun _ -> raise (exn "this should have been an UndefinedState Error"))
        (fun e -> Assert.True( match e with | UndefinedState _ -> true | _ -> false))
    |> ignore

[<Fact>]
let ``unset terminal state`` () =
    testWorkflow
    |> processCmd (addState testWorkflowId "New Terminal")
    |> processCmd (setTerminalState testWorkflowId 2u)
    |> processCmd (unSetTerminalState testWorkflowId 2u)
    |> someWorkflow
    |> Result.map(fun s ->
        Assert.False(s.TerminalStates.Contains 2u)
        Assert.False((s.States |> Map.find 2u).IsTerminal))
    |> raiseIfError

[<Fact>]
let ``unset terminal state that is not terminal generates no events`` () =
    testWorkflow
    |> processCmd (unSetTerminalState testWorkflowId 1u)
    |> Result.bind(Workflow.handle (UnSetTerminalState { WorkflowId = testWorkflowId; StateId = 1u }))
    |> Result.map(Seq.isEmpty >> Assert.True)
    |> raiseIfError

[<Fact>]
let ``unsetting terminal state that doesn't exist`` () =
    testWorkflow
    |> processCmd (unSetTerminalState testWorkflowId 2u)
    |> Result.map2
        (fun _ -> raise (exn "this should have been an UndefinedState Error"))
        (fun e -> Assert.True( match e with | UndefinedState _ -> true | _ -> false))
    |> ignore

[<Fact>]
let ``add transition`` () =
    testWorkflow
    |> processCmd (addState testWorkflowId "New Terminal")
    |> processCmd (addTransition testWorkflowId "New Transition" 1u 2u)
    |> someWorkflow
    |> Result.map(fun s ->
        Assert.Equal(2,s.Transitions.Count)
        let newT = s.Transitions |> Map.find 1u
        Assert.Equal(1u,newT.Id)
        Assert.Equal("New Transition",newT.Name)
        Assert.Equal(1u,newT.SourceState)
        Assert.Equal(2u,newT.TargetState))
    |> raiseIfError

[<Fact>]
let ``add duplicate transition path`` () =
    testWorkflow
    |> processCmd (addState testWorkflowId "New Terminal")
    |> processCmd (addTransition testWorkflowId "New Transition" 1u 2u)
    |> processCmd (addTransition testWorkflowId "New Transition 2" 1u 2u)
    |> someWorkflow
    |> Result.map2
        (fun _ -> raise (exn "this should have been an DuplicateTransition Error"))
        (fun e -> Assert.True( match e with | DuplicateTransition _ -> true | _ -> false))
    |> ignore

[<Fact>]
let ``add duplicate transition name`` () =
    testWorkflow
    |> processCmd (addState testWorkflowId "New State")
    |> processCmd (addTransition testWorkflowId "New Transition" 1u 2u)
    |> processCmd (addState testWorkflowId "New State 2")
    |> processCmd (addTransition testWorkflowId "New Transition" 0u 3u)
    |> someWorkflow
    |> Result.map2
        (fun _ -> raise (exn "this should have been an DuplicateTransitionName Error"))
        (fun e -> Assert.True( match e with | DuplicateTransitionName _ -> true | _ -> false))
    |> ignore

[<Fact>]
let ``transition from nonexistant state`` () =
    testWorkflow
    |> processCmd (addTransition testWorkflowId "New Transition" 2u 1u)
    |> someWorkflow
    |> Result.map2
        (fun _ -> raise (exn "this should have been an UndefinedState Error"))
        (fun e -> Assert.True( match e with | UndefinedState _ -> true | _ -> false))
    |> ignore

[<Fact>]
let ``transition to nonexistant state`` () =
    testWorkflow
    |> processCmd (addTransition testWorkflowId "New Transition" 1u 2u)
    |> someWorkflow
    |> Result.map2
        (fun _ -> raise (exn "this should have been an UndefinedState Error"))
        (fun e -> Assert.True( match e with | UndefinedState _ -> true | _ -> false))
    |> ignore

[<Fact>]
let ``edit transition`` () =
    testWorkflow
    |> processCmd (addState testWorkflowId "New State 2")
    |> processCmd (addTransition testWorkflowId "New Transition" 0u 2u)
    |> processCmd (addState testWorkflowId "New State 3")
    |> processCmd (addTransition testWorkflowId "Edited Transition" 2u 3u)
    |> someWorkflow
    |> Result.map(fun s ->
        Assert.Equal(3,s.Transitions.Count)
        let newT = s.Transitions |> Map.find 2u
        Assert.Equal(2u,newT.Id)
        Assert.Equal("Edited Transition",newT.Name)
        Assert.Equal(2u,newT.SourceState)
        Assert.Equal(3u,newT.TargetState))
    |> raiseIfError

[<Fact>]
let ``edit transition that doesn't exist`` () =
    testWorkflow
    |> processCmd (editTransition testWorkflowId 1u "Doesn't Exist" 0u 1u)
    |> someWorkflow
    |> Result.map2
        (fun _ -> raise (exn "this should have been an UndefinedTransition Error"))
        (fun e -> Assert.True( match e with | UndefinedTransition _ -> true | _ -> false))
    |> ignore

[<Fact>]
let ``edit transition to duplicate name`` () =
    testWorkflow
    |> processCmd (addState testWorkflowId "New State 2")
    |> processCmd (addTransition testWorkflowId "New Transition" 1u 2u)
    |> processCmd (addState testWorkflowId "New State 3")
    |> processCmd (addTransition testWorkflowId "New Transition 2" 2u 3u)
    |> processCmd (editTransition testWorkflowId 2u "New Transition" 2u 3u)
    |> someWorkflow
    |> Result.map2
        (fun _ -> raise (exn "this should have been an DuplicateTransitionName Error"))
        (fun e -> Assert.True( match e with | DuplicateTransitionName _ -> true | _ -> false))
    |> ignore

[<Fact>]
let ``edit transition to duplicate path`` () =
    testWorkflow
    |> processCmd (addState testWorkflowId "New State 2")
    |> processCmd (addTransition testWorkflowId "New Transition" 1u 2u)
    |> processCmd (addState testWorkflowId "New State 3")
    |> processCmd (addTransition testWorkflowId "New Transition 2" 2u 3u)
    |> processCmd (editTransition testWorkflowId 2u "New Transition 2" 1u 2u)
    |> someWorkflow
    |> Result.map2
        (fun _ -> raise (exn "this should have been an DuplicateTransition Error"))
        (fun e -> Assert.True( match e with | DuplicateTransition _ -> true | _ -> false))
    |> ignore

[<Fact>]
let ``edit transition to nonexistant source state`` () =
    testWorkflow
    |> processCmd (addState testWorkflowId "New State 2")
    |> processCmd (addTransition testWorkflowId "New Transition" 1u 2u)
    |> processCmd (editTransition testWorkflowId 1u "New Transition" 99u 2u)
    |> someWorkflow
    |> Result.map2
        (fun _ -> raise (exn "this should have been an UndefinedState Error"))
        (fun e -> Assert.True( match e with | UndefinedState _ -> true | _ -> false))
    |> ignore

[<Fact>]
let ``edit transition to nonexistant target state`` () =
    testWorkflow
    |> processCmd (addState testWorkflowId "New State 2")
    |> processCmd (addTransition testWorkflowId "New Transition" 1u 2u)
    |> processCmd (editTransition testWorkflowId 1u "New Transition" 1u 3u)
    |> someWorkflow
    |> Result.map2
        (fun _ -> raise (exn "this should have been an UndefinedState Error"))
        (fun e -> Assert.True( match e with | UndefinedState _ -> true | _ -> false))
    |> ignore

[<Fact>]
let ``drop transition`` () =
    testWorkflow
    |> processCmd (dropTransition testWorkflowId 0u)
    |> someWorkflow
    |> Result.map(fun m -> Assert.Equal(0,m.Transitions.Count))
    |> raiseIfError

[<Fact>]
let ``drop transition that doesn't exist`` () =
    testWorkflow
    |> processCmd (dropTransition testWorkflowId 1u)
    |> someWorkflow
    |> Result.map2
        (fun _ -> raise (exn "this should have been an UndefinedTransition Error"))
        (fun e -> Assert.True( match e with | UndefinedTransition _ -> true | _ -> false))
    |> ignore
