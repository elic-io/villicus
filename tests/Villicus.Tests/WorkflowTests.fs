module WorkflowTests

open System
open Xunit
open Villicus.Domain
open TestUtil

let argNull = expectExn<ArgumentNullException>

[<Fact>]
let ``CreateCommand missing name`` () =    
    argNull <| fun () -> CreateWorkflowCommand (Guid.NewGuid() |> WorkflowId, "") |> ignore
    argNull <| fun () -> CreateWorkflowCommand (Guid.NewGuid() |> WorkflowId, null) |> ignore
    argNull <| fun () -> CreateWorkflowCommand (Guid.NewGuid() |> WorkflowId, " ") |> ignore

[<Fact>]
let ``AddStateCommand missing name`` () =
    argNull <| fun () -> AddStateCommand (Guid.NewGuid() |> WorkflowId, "") |> ignore

[<Fact>]
let ``EditStateCommand missing name`` () =
    argNull <| fun () -> EditStateCommand (Guid.NewGuid() |> WorkflowId, 0u, "") |> ignore

[<Fact>]
let ``AddTransitionCommand missing name`` () =
    argNull <| fun () -> AddTransitionCommand (Guid.NewGuid() |> WorkflowId, "", 0u, 1u) |> ignore

[<Fact>]
let ``AddTransitionCommand duplicate states`` () =
    expectExn<ArgumentException> <| fun () -> AddTransitionCommand (Guid.NewGuid() |> WorkflowId, "Duplicate States", 0u, 0u) |> ignore

[<Fact>]
let ``EditTransitionCommand missing name`` () =
    argNull <| fun () -> EditTransitionCommand (Guid.NewGuid() |> WorkflowId, 0u, "", 0u, 1u) |> ignore

[<Fact>]
let ``EditTransitionCommand duplicate states`` () =
    expectExn<ArgumentException> <| fun () -> EditTransitionCommand (Guid.NewGuid() |> WorkflowId, 0u, "Duplicate States", 0u, 0u) |> ignore

let handleEvolve command state =
    state
    |> Workflow.handle command
    |> Result.map (List.fold Workflow.evolve state)

let processCmd command = handleEvolve command |> Result.bind

let testCreateWorkflow iD name = handleEvolve (CreateWorkflowCommand (iD, name) |> CreateWorkflow) None 

let newWorkflowId = Guid.NewGuid() |> WorkflowId
let testWorkFlowName = "TestWorkflow"
let testWorkflow = testCreateWorkflow newWorkflowId testWorkFlowName

[<Fact>]
let ``workflow creation`` () =
    None |> Workflow.handle (CreateWorkflowCommand (newWorkflowId, testWorkFlowName) |> CreateWorkflow)
    |> Result.map (fun events -> 
        Assert.Equal(4,events.Length)
        List.fold Workflow.evolve None events)
    |> Result.map(fun wf -> Assert.True(wf.IsSome); wf)
    |> Result.map (Option.map (fun x ->
                Assert.Equal(testWorkFlowName,x.Name)
                Assert.Equal(newWorkflowId,x.WorkflowId)))
    |> Result.mapError(fun e -> raise e)
    |> ignore

[<Fact>]
let ``duplicate workflow error`` () =
    fun () ->
        testWorkflow
        |> processCmd (CreateWorkflowCommand (Guid.NewGuid() |> WorkflowId, "Test2") |> CreateWorkflow)
        |> Result.mapError(fun e -> raise e)
        |> ignore
    |> expectExn<DuplicateWorkflowIdException>

[<Fact>]
let ``simple created workflow is valid`` () =
    match testWorkflow with
      | Ok model ->
        model 
        |> Option.bind (fun m -> 
            Workflow.problems m
            |> Option.map (fun problems ->
                raise (InvalidWorkflowException("Initially Created Workflow is Invalid",m.WorkflowId,problems))))
      | Error e -> raise e
    |> ignore

[<Fact>]
let ``publish workflow`` () =
    testWorkflow
    |> Result.map(Option.map(fun before ->
        testWorkflow
        |> processCmd (PublishWorkflow newWorkflowId)
        |> Result.map(Option.map(fun m ->
            Assert.True(before.Version |> m.PublishedVersions.Contains)
            Assert.Equal(before.Version.Inc,m.Version)
            Assert.Equal(2,m.Versions.Count)))
        |> Result.mapError(fun e -> raise e)))
    |> ignore

[<Fact>]
let ``republish workflow on an unpublished workflow is the same as publish`` () =
    testWorkflow
    |> Result.map(Option.map(fun before ->
        testWorkflow
        |> processCmd (RePublishWorkflow before.VersionedWorkflowId)
        |> Result.map(Option.map(fun m ->
            Assert.True(before.Version |> m.PublishedVersions.Contains)
            Assert.Equal(before.Version.Inc,m.Version)))
        |> Result.mapError(fun e -> raise e)))
    |> ignore

[<Fact>]
let ``cannot republish a version that doesn't exist`` () =
    fun () ->
        testWorkflow
        |> Result.map(Option.map(fun before ->
            testWorkflow
            |> processCmd (RePublishWorkflow { Id = before.WorkflowId; Version = before.Version.Inc })
            |> Result.mapError(fun e -> raise e)))
        |> ignore
    |> expectExn<UndefinedVersionException>

let dec = (fun (Version i) -> i-1UL |> Version)

[<Fact>]
let ``republish a published version generates empty event set`` () =
    let published =
        testWorkflow
        |> processCmd (PublishWorkflow newWorkflowId)
    published |> Result.map(Option.map(fun p -> 
            let pubVersionId = { 
                Id = p.WorkflowId 
                Version = p.PublishedVersions |> Set.toSeq |> Seq.head }
            published
            |> Result.bind (Workflow.handle (RePublishWorkflow pubVersionId))
            |> Result.map(fun events -> Assert.True(events |> List.isEmpty))
            |> Result.mapError(fun e -> raise e)))
    |> ignore

[<Fact>]
let ``withdraw a version`` () =
    let published =
        testWorkflow
        |> processCmd (PublishWorkflow newWorkflowId)
    published |> Result.map(Option.map(fun p -> 
            let pubVersion = p.PublishedVersions |> Set.toSeq |> Seq.head
            let pubVersionId = { Id = p.WorkflowId; Version = pubVersion }
            published
            |> processCmd (WithdrawWorkflow pubVersionId)
            |> Result.map(Option.map(fun w -> Assert.False(w.PublishedVersions.Contains pubVersion)))
            |> Result.mapError(fun e -> raise e)))
    |> ignore

[<Fact>]
let ``republish a withdrawn version`` () =
    let published =
        testWorkflow
        |> processCmd (PublishWorkflow newWorkflowId)
    published |> Result.map(Option.map(fun p -> 
            let pubVersion = p.PublishedVersions |> Set.toSeq |> Seq.head
            let pubVersionId = { Id = p.WorkflowId; Version = pubVersion }
            published
            |> processCmd (WithdrawWorkflow pubVersionId)
            |> processCmd (RePublishWorkflow pubVersionId)
            |> Result.map(Option.map(fun w -> Assert.True(pubVersion |> w.PublishedVersions.Contains)))
            |> Result.mapError(fun e -> raise e)))
    |> ignore

[<Fact>]
let ``add state`` () =
    testWorkflow
    |> processCmd (AddState (AddStateCommand(newWorkflowId,"Orphan")))
    |> Result.bind("testWorkflow should not be None" |> exn |> Result.ofOption)
    |> Result.map(fun s ->
        Assert.Equal(3,s.States.Count)
        Assert.Equal("Orphan",Map.find 2u s.States |> (fun x -> x.Name)))
    |> Result.mapError(fun e -> raise e)
    |> ignore

[<Fact>]
let ``add state with same name as existing`` () =
    fun () ->
        testWorkflow
        |> processCmd (AddState (AddStateCommand(newWorkflowId,"Orphan")))
        |> processCmd (AddState (AddStateCommand(newWorkflowId,"Orphan")))
        |> Result.bind("testWorkflow should not be None" |> exn |> Result.ofOption)
        |> Result.mapError(fun e -> raise e)
        |> ignore
    |> expectExn<DuplicateStateNameException>

[<Fact>]
let ``rename state`` () =
    testWorkflow
    |> processCmd (RenameState (EditStateCommand(newWorkflowId,0u,"Renamed Initial State")))
    |> Result.bind("testWorkflow should not be None" |> exn |> Result.ofOption)
    |> Result.map(fun s ->
        Assert.Equal(2,s.States.Count)
        Assert.Equal("Renamed Initial State",Map.find 0u s.States |> (fun x -> x.Name)))
    |> Result.mapError(fun e -> raise e)
    |> ignore

[<Fact>]
let ``rename state that doesn't exist`` () =
    fun () ->
        testWorkflow
        |> processCmd (RenameState (EditStateCommand(newWorkflowId,2u,"Rename NonExistant State")))
        |> Result.bind("testWorkflow should not be None" |> exn |> Result.ofOption)
        |> Result.mapError(fun e -> raise e)
        |> ignore
    |> expectExn<UndefinedStateException>

[<Fact>]
let ``can't publish invalid workflow`` () =
    fun () ->
        testWorkflow
        |> processCmd (AddState (AddStateCommand(newWorkflowId,"Orphan")))
        |> processCmd (PublishWorkflow newWorkflowId)
        |> Result.mapError(fun e -> raise e)
        |> ignore
    |> expectExn<InvalidWorkflowException>

[<Fact>]
let ``orphaned state makes workflow invalid`` () =
    testWorkflow
    |> processCmd (AddState (AddStateCommand(newWorkflowId,"Orphan")))
    |> function
      | Ok model ->
        model 
        |> Option.bind (fun m -> 
            Assert.True((Workflow.problems m).IsSome)
            Workflow.problems m
            |> Option.map (fun p -> 
                Assert.False(p.Valid)
                Assert.True(2u |> p.UnreachableStates.Contains)))
      | Error e -> raise e
    |> ignore

[<Fact>]
let ``state that can't reach terminal makes workflow invalid`` () =
    testWorkflow
    |> processCmd (AddState (AddStateCommand(newWorkflowId,"Not Terminated")))
    |> processCmd (AddTransition (AddTransitionCommand(newWorkflowId,"to nonTerminated",0u,2u)))
    |> function
      | Ok model ->
        model 
        |> Option.bind (fun m -> 
            Assert.True((Workflow.problems m).IsSome)
            Workflow.problems m
            |> Option.map (fun p -> 
                Assert.False(p.Valid)
                Assert.False(p.UnreachableStates.Contains 2u)
                Assert.True(p.CannotReachAnyTerminalState.Contains 2u)))
      | Error e -> raise e
    |> ignore

[<Fact>]
let ``workflow without terminal states is invalid`` () =
    testWorkflow
    |> processCmd (UnSetTerminalState { WorkflowId = newWorkflowId; StateId = 1u } )
    |> function
      | Ok model ->
        model 
        |> Option.bind (fun m -> 
            Assert.True((Workflow.problems m).IsSome)
            Workflow.problems m
            |> Option.map (fun p -> 
                Assert.False(p.Valid)
                Assert.False(p.UnreachableStates.Contains 1u)
                Assert.True(p.CannotReachAnyTerminalState.Contains 0u)
                Assert.True(p.CannotReachAnyTerminalState.Contains 1u)
                Assert.True(p.NoTerminalStates)))
      | Error e -> raise e
    |> ignore

// since this is an extreme edge case, we're probably alright
[<Fact (Skip="Takes too long to run every time")>]
let ``workflow state count exceeded returns correct error`` () =
    let rec processAll i limit a =
        let b = a |> processCmd (AddState (AddStateCommand(newWorkflowId,sprintf "State %u" i)))
        if i = limit then b else processAll (i+1u) limit b
    fun () ->
        testWorkflow
        |> processAll 2u System.UInt32.MaxValue
        |> processCmd (AddState (AddStateCommand(newWorkflowId,sprintf "One State too many")))
        |> Result.mapError(fun e -> raise e)
        |> ignore
    |> expectExn<MaxCountExceededException>

[<Fact>]
let ``copy Workflow`` () =
    let oldWorkflow =
        match testWorkflow with
            | Ok twf -> match twf with | Some x -> x | None -> failwith "won't happen"
            | Error ex -> raise ex
    let copyCmd =
        CopyWorkflowCommand({ Id = newWorkflowId; Version = 1UL |> Version },
          Guid.NewGuid() |> WorkflowId,
          oldWorkflow.Name |> sprintf "Copy of %s")
        |> CopyWorkflow

    let events = testWorkflow |> Result.bind(Workflow.handle copyCmd)
    let origEvents = events |> Result.map(List.filter(fun e -> oldWorkflow.WorkflowId = Workflow.eWorkflowId e ))
    let copyEvents = events |> Result.map(List.filter(fun e -> oldWorkflow.WorkflowId <> Workflow.eWorkflowId e ))
    testWorkflow |> Result.bind(fun state ->
        origEvents |> Result.map (List.fold Workflow.evolve state))
    |> Result.map(fun origWF ->
        copyEvents |> Result.map (List.fold Workflow.evolve None)
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
    |> Result.mapError(fun e -> raise e)
    |> ignore

[<Fact>]
let ``drop state`` () =
    testWorkflow
    |> processCmd (DropState { WorkflowId = newWorkflowId; StateId = 1u })
    |> Result.bind("testWorkflow should not be None" |> exn |> Result.ofOption)
    |> Result.map(fun s ->
        Assert.Equal(1,s.States.Count))
    |> Result.mapError(fun e -> raise e)
    |> ignore

[<Fact>]
let ``can't drop initial state`` () =
    fun () ->
        testWorkflow
        |> processCmd (DropState { WorkflowId = newWorkflowId; StateId = 0u })
        |> Result.bind("testWorkflow should not be None" |> exn |> Result.ofOption)
        |> Result.mapError(fun e -> raise e)
        |> ignore
    |> expectExn<InitialStateException>

[<Fact>]
let ``can't drop nonexistant state`` () =
    fun () ->
        testWorkflow
        |> processCmd (DropState { WorkflowId = newWorkflowId; StateId = 2u })
        |> Result.bind("testWorkflow should not be None" |> exn |> Result.ofOption)
        |> Result.mapError(fun e -> raise e)
        |> ignore
    |> expectExn<UndefinedStateException>

[<Fact>]
let ``set terminal state`` () =
    testWorkflow
    |> processCmd (AddState (AddStateCommand(newWorkflowId,"New Terminal")))
    |> processCmd (SetTerminalState { WorkflowId = newWorkflowId; StateId = 2u })
    |> Result.bind("testWorkflow should not be None" |> exn |> Result.ofOption)
    |> Result.map(fun s ->
        let newTerminal = s.States |> Map.find 2u
        Assert.True(s.TerminalStates.Contains 2u)
        Assert.True(newTerminal.IsTerminal))
    |> Result.mapError(fun e -> raise e)
    |> ignore

[<Fact>]
let ``set terminal state that is already terminal generates no events`` () =
    testWorkflow
    |> processCmd (AddState (AddStateCommand(newWorkflowId,"New Terminal")))
    |> processCmd (SetTerminalState { WorkflowId = newWorkflowId; StateId = 2u })
    |> Result.bind(Workflow.handle (SetTerminalState { WorkflowId = newWorkflowId; StateId = 2u }))
    |> Result.map(List.isEmpty >> Assert.True)
    |> Result.mapError(fun e -> raise e)
    |> ignore

[<Fact>]
let ``Setting terminal state that doesn't exist`` () =
    fun () ->
        testWorkflow
        |> processCmd (SetTerminalState { WorkflowId = newWorkflowId; StateId = 2u })
        |> Result.mapError(fun e -> raise e)
        |> ignore
    |> expectExn<UndefinedStateException>

[<Fact>]
let ``unset terminal state`` () =
    testWorkflow
    |> processCmd (AddState (AddStateCommand(newWorkflowId,"New Terminal")))
    |> processCmd (SetTerminalState { WorkflowId = newWorkflowId; StateId = 2u })
    |> processCmd (UnSetTerminalState { WorkflowId = newWorkflowId; StateId = 2u })
    |> Result.bind("testWorkflow should not be None" |> exn |> Result.ofOption)
    |> Result.map(fun s ->
        Assert.False(s.TerminalStates.Contains 2u)
        Assert.False((s.States |> Map.find 2u).IsTerminal))
    |> Result.mapError(fun e -> raise e)
    |> ignore

[<Fact>]
let ``unset terminal state that is not terminal generates no events`` () =
    testWorkflow
    |> processCmd (UnSetTerminalState { WorkflowId = newWorkflowId; StateId = 1u })
    |> Result.bind(Workflow.handle (UnSetTerminalState { WorkflowId = newWorkflowId; StateId = 1u }))
    |> Result.map(List.isEmpty >> Assert.True)
    |> Result.mapError(fun e -> raise e)
    |> ignore

[<Fact>]
let ``unsetting terminal state that doesn't exist`` () =
    fun () ->
        testWorkflow
        |> processCmd (UnSetTerminalState { WorkflowId = newWorkflowId; StateId = 2u })
        |> Result.mapError(fun e -> raise e)
        |> ignore
    |> expectExn<UndefinedStateException>

[<Fact>]
let ``add transition`` () =
    testWorkflow
    |> processCmd (AddState (AddStateCommand (newWorkflowId, "New State")))
    |> processCmd (AddTransition (AddTransitionCommand (newWorkflowId, "New Transition", 1u, 2u)))
    |> Result.bind("testWorkflow should not be None" |> exn |> Result.ofOption)
    |> Result.map(fun s ->
        Assert.Equal(2,s.Transitions.Count)
        let newT = s.Transitions |> Map.find 1u
        Assert.Equal(1u,newT.Id)
        Assert.Equal("New Transition",newT.Name)
        Assert.Equal(1u,newT.SourceState)
        Assert.Equal(2u,newT.TargetState))
    |> Result.mapError(fun e -> raise e)
    |> ignore

[<Fact>]
let ``add duplicate transition path`` () =
    fun () ->
        testWorkflow
        |> processCmd (AddState (AddStateCommand (newWorkflowId, "New State")))
        |> processCmd (AddTransition (AddTransitionCommand (newWorkflowId, "New Transition", 1u, 2u)))
        |> processCmd (AddTransition (AddTransitionCommand (newWorkflowId, "New Transition 2", 1u, 2u)))
        |> Result.bind("testWorkflow should not be None" |> exn |> Result.ofOption)
        |> Result.mapError(fun e -> raise e)
        |> ignore
    |> expectExn<DuplicateTransitionException>

[<Fact>]
let ``add duplicate transition name`` () =
    fun () ->
        testWorkflow
        |> processCmd (AddState (AddStateCommand (newWorkflowId, "New State")))
        |> processCmd (AddTransition (AddTransitionCommand (newWorkflowId, "New Transition", 1u, 2u)))
        |> processCmd (AddState (AddStateCommand (newWorkflowId, "New State 2")))
        |> processCmd (AddTransition (AddTransitionCommand (newWorkflowId, "New Transition", 0u, 3u)))
        |> Result.bind("testWorkflow should not be None" |> exn |> Result.ofOption)
        |> Result.mapError(fun e -> raise e)
        |> ignore
    |> expectExn<DuplicateTransitionNameException>

[<Fact>]
let ``transition from nonexistant state`` () =
    fun () ->
        testWorkflow
        |> processCmd (AddTransition (AddTransitionCommand (newWorkflowId, "New Transition", 2u, 1u)))
        |> Result.bind("testWorkflow should not be None" |> exn |> Result.ofOption)
        |> Result.mapError(fun e -> raise e)
        |> ignore
    |> expectExn<UndefinedStateException>

[<Fact>]
let ``transition to nonexistant state`` () =
    fun () ->
        testWorkflow
        |> processCmd (AddTransition (AddTransitionCommand (newWorkflowId, "New Transition", 1u, 2u)))
        |> Result.bind("testWorkflow should not be None" |> exn |> Result.ofOption)
        |> Result.mapError(fun e -> raise e)
        |> ignore
    |> expectExn<UndefinedStateException>

[<Fact>]
let ``edit transition`` () =
    testWorkflow
    |> processCmd (AddState (AddStateCommand (newWorkflowId, "New State 2")))
    |> processCmd (AddTransition (AddTransitionCommand (newWorkflowId, "New Transition", 1u, 2u)))
    |> processCmd (AddState (AddStateCommand (newWorkflowId, "New State 3")))
    |> processCmd (EditTransition (EditTransitionCommand (newWorkflowId, 1u, "Edited Transition", 2u, 3u)))
    |> Result.bind("testWorkflow should not be None" |> exn |> Result.ofOption)
    |> Result.map(fun s ->
        Assert.Equal(2,s.Transitions.Count)
        let newT = s.Transitions |> Map.find 1u
        Assert.Equal(1u,newT.Id)
        Assert.Equal("Edited Transition",newT.Name)
        Assert.Equal(2u,newT.SourceState)
        Assert.Equal(3u,newT.TargetState))
    |> Result.mapError(fun e -> raise e)
    |> ignore

[<Fact>]
let ``edit transition that doesn't exist`` () =
    fun () ->
        testWorkflow
        |> processCmd (EditTransition (EditTransitionCommand (newWorkflowId, 1u, "Doesn't Exist", 0u, 1u)))
        |> Result.bind("testWorkflow should not be None" |> exn |> Result.ofOption)
        |> Result.mapError(fun e -> raise e)
        |> ignore
    |> expectExn<UndefinedTransitionException>

[<Fact>]
let ``edit transition to duplicate name`` () =
    fun () ->
        testWorkflow
        |> processCmd (AddState (AddStateCommand (newWorkflowId, "New State 2")))
        |> processCmd (AddTransition (AddTransitionCommand (newWorkflowId, "New Transition", 1u, 2u)))
        |> processCmd (AddState (AddStateCommand (newWorkflowId, "New State 3")))
        |> processCmd (AddTransition (AddTransitionCommand (newWorkflowId, "New Transition 2", 2u, 3u)))
        |> processCmd (EditTransition (EditTransitionCommand (newWorkflowId, 2u, "New Transition", 2u, 3u)))
        |> Result.bind("testWorkflow should not be None" |> exn |> Result.ofOption)
        |> Result.mapError(fun e -> raise e)
        |> ignore
    |> expectExn<DuplicateTransitionNameException>

[<Fact>]
let ``edit transition to duplicate path`` () =
    fun () ->
        testWorkflow
        |> processCmd (AddState (AddStateCommand (newWorkflowId, "New State 2")))
        |> processCmd (AddTransition (AddTransitionCommand (newWorkflowId, "New Transition", 1u, 2u)))
        |> processCmd (AddState (AddStateCommand (newWorkflowId, "New State 3")))
        |> processCmd (AddTransition (AddTransitionCommand (newWorkflowId, "New Transition 2", 2u, 3u)))
        |> processCmd (EditTransition (EditTransitionCommand (newWorkflowId, 2u, "New Transition 2", 1u, 2u)))
        |> Result.bind("testWorkflow should not be None" |> exn |> Result.ofOption)
        |> Result.mapError(fun e -> raise e)
        |> ignore
    |> expectExn<DuplicateTransitionException>

[<Fact>]
let ``edit transition to nonexistant source state`` () =
    fun () ->
        testWorkflow
        |> processCmd (AddState (AddStateCommand (newWorkflowId, "New State 2")))
        |> processCmd (AddTransition (AddTransitionCommand (newWorkflowId, "New Transition", 1u, 2u)))
        |> processCmd (EditTransition (EditTransitionCommand (newWorkflowId, 1u, "New Transition", 99u, 2u)))
        |> Result.bind("testWorkflow should not be None" |> exn |> Result.ofOption)
        |> Result.mapError(fun e -> raise e)
        |> ignore
    |> expectExn<UndefinedStateException>

[<Fact>]
let ``edit transition to nonexistant target state`` () =
    fun () ->
        testWorkflow
        |> processCmd (AddState (AddStateCommand (newWorkflowId, "New State 2")))
        |> processCmd (AddTransition (AddTransitionCommand (newWorkflowId, "New Transition", 1u, 2u)))
        |> processCmd (EditTransition (EditTransitionCommand (newWorkflowId, 1u, "New Transition", 1u, 3u)))
        |> Result.bind("testWorkflow should not be None" |> exn |> Result.ofOption)
        |> Result.mapError(fun e -> raise e)
        |> ignore
    |> expectExn<UndefinedStateException>

[<Fact>]
let ``drop transition`` () =
    testWorkflow
    |> processCmd (DropTransition { WorkflowId = newWorkflowId; TransitionId = 0u })
    |> Result.bind("testWorkflow should not be None" |> exn |> Result.ofOption)
    |> Result.map(fun m -> Assert.Equal(0,m.Transitions.Count))
    |> Result.mapError(fun e -> raise e)
    |> ignore

[<Fact>]
let ``drop transition that doesn't exist`` () =
    fun () ->
        testWorkflow
        |> processCmd (DropTransition { WorkflowId = newWorkflowId; TransitionId = 1u })
        |> Result.bind("testWorkflow should not be None" |> exn |> Result.ofOption)
        |> Result.mapError(fun e -> raise e)
        |> ignore
    |> expectExn<UndefinedTransitionException>
