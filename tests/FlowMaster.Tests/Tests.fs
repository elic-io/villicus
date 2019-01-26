module Tests

open System
open Xunit
open FlowMaster.Domain.Workflow
open FlowMaster.ResultBuilder
open Xunit.Sdk


//TODO: Look into using Assert.Raises instead?
let inline expectExn<'e when 'e :> exn> (f:unit->unit) =
    try
        f ()
    with 
      :? 'e -> ()
      | e -> raise e

let argNull = expectExn<ArgumentNullException>

[<Fact>]
let ``Expect Exception Works`` () =
    expectExn<Exception> <| fun () -> failwith "generic Exception"

[<Fact>]
let ``CreateCommand missing name`` () =    
    argNull <| fun () -> CreateCommand (Guid.NewGuid() |> WorkflowId, "") |> ignore
    argNull <| fun () -> CreateCommand (Guid.NewGuid() |> WorkflowId, null) |> ignore
    argNull <| fun () -> CreateCommand (Guid.NewGuid() |> WorkflowId, " ") |> ignore

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
    |> handle command
    |> Result.map (List.fold evolve state)

let processCmd command = handleEvolve command |> Result.bind

let testCreateWorkflow iD name = handleEvolve (CreateCommand (iD, name) |> CreateWorkflow) None 

let newWorkflowId = Guid.NewGuid() |> WorkflowId
let testWorkFlowName = "TestWorkflow"
let testWorkflow = testCreateWorkflow newWorkflowId testWorkFlowName

[<Fact>]
let ``workflow creation`` () =
    None |> handle (CreateCommand (newWorkflowId, testWorkFlowName) |> CreateWorkflow)
    |> Result.map (fun events -> 
        Assert.Equal(4,events.Length)
        List.fold evolve None events)
    |> Result.map(fun wf -> Assert.True(wf.IsSome); wf)
    |> Result.map (fun x -> Assert.True(x.IsSome); x)
    |> Result.map (Option.map (fun x ->
                Assert.Equal(testWorkFlowName,x.Name)
                Assert.Equal(newWorkflowId,x.WorkflowId)))
    |> Result.mapError(fun e -> raise e)
    |> ignore

[<Fact>]
let ``duplicate workflow error`` () =
    fun () ->
        testWorkflow
        |> processCmd (CreateCommand (Guid.NewGuid() |> WorkflowId, "Test2") |> CreateWorkflow)
        |> ignore
    |> expectExn<DuplicateWorkflowIdException>

[<Fact>]
let ``simple created workflow is valid`` () =
    match testWorkflow with
      | Ok model ->
        model 
        |> Option.bind (fun m -> 
            m.Problems
            |> Option.map (fun problems ->
                raise (InvalidWorkflowException.New problems "Initially Created Workflow is Invalid")))
      | Error e -> raise e
    |> ignore

[<Fact>]
let ``publish workflow`` () =
    testWorkflow
    |> Result.bind (fun x ->
        Assert.True(x.IsSome)
        x |> handleEvolve (PublishWorkflow newWorkflowId))
    |> Result.mapError(fun e -> raise e)
    |> ignore

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
            Assert.True(m.Problems.IsSome)
            m.Problems
            |> Option.map (fun problems -> 
                Assert.False(problems.Valid)
                Assert.True(2u |> problems.UnreachableStates.Contains)))
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
            Assert.True(m.Problems.IsSome)
            m.Problems
            |> Option.map (fun problems -> 
                Assert.False(problems.Valid)
                Assert.False(problems.UnreachableStates.Contains 2u)
                Assert.True(problems.CannotReachAnyTerminalState.Contains 2u)))
      | Error e -> raise e
    |> ignore

[<Fact>]
let ``workflow without terminal states is invalid`` () =
    testWorkflow
    |> processCmd (UnSetTerminalState { WorkflowId = newWorkflowId; State = 1u } )
    |> function
      | Ok model ->
        model 
        |> Option.bind (fun m -> 
            Assert.True(m.Problems.IsSome)
            m.Problems
            |> Option.map (fun problems -> 
                Assert.False(problems.Valid)
                Assert.False(problems.UnreachableStates.Contains 1u)
                Assert.True(problems.CannotReachAnyTerminalState.Contains 0u)
                Assert.True(problems.CannotReachAnyTerminalState.Contains 1u)
                Assert.True(problems.NoTerminalStates)))
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
        { Target = Guid.NewGuid() |> WorkflowId
          Source = { Id = newWorkflowId; Version = 1UL |> Version }
          CopyName = oldWorkflow.Name |> sprintf "Copy of %s" }
        |> CopyWorkflow

    fun () ->
        let events = testWorkflow |> Result.bind(handle copyCmd)
        let origEvents = events |> Result.map(List.filter(fun e -> oldWorkflow.WorkflowId = eWorkflowId e ))
        let copyEvents = events |> Result.map(List.filter(fun e -> oldWorkflow.WorkflowId <> eWorkflowId e ))
        testWorkflow |> Result.bind(fun state ->
            origEvents |> Result.map (List.fold evolve state))
        |> Result.map(fun origWF ->
            copyEvents |> Result.map (List.fold evolve None)
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
