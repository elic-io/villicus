module JourneyTests

open System
open Xunit
open FlowMaster.Domain
open FlowMaster.Domain.Journey
open TestUtil

let subject = Guid.NewGuid() :> obj
let newJourneyId = Guid.NewGuid() |> JourneyId
let testWorkflow =
    WorkflowTests.testWorkflow
    |> WorkflowTests.processCmd (Workflow.AddTransition (Workflow.AddTransitionCommand(WorkflowTests.newWorkflowId,"Reactivate",1u,0u)))
    |> Result.bind(Result.ofOption (exn "workflow doesn't exist"))

let lookup vwid =
    testWorkflow
    |> Result.bind(fun w ->
        let v = w.VersionedWorkflowId
        match vwid = v with
          | true -> Ok w
          | false -> Workflow.UndefinedVersionException(v.Id,v.Version) :> exn |> Error)
    //|> Result.mapError(fun e -> raise e)

let createTest (workflow:Result<Workflow.WorkflowModel,exn>) =
    workflow
    |> Result.bind(fun wf -> 
        let createCmd = { 
            JourneyId = newJourneyId
            VersionedWorkflowId = wf.VersionedWorkflowId
            Subject = subject }
        createJourney lookup createCmd NonExistingJourney)

[<Fact>]
let ``create journey`` () =
    testWorkflow
    |> Result.bind(fun wf -> 
        let createCmd = { 
            JourneyId = newJourneyId
            VersionedWorkflowId = wf.VersionedWorkflowId
            Subject = subject }
        createJourney lookup createCmd NonExistingJourney
        |> Result.map(List.fold (evolve wf) NonExistingJourney)
        |> Result.bind (toResult newJourneyId)
        |> Result.map(fun j -> 
            Assert.Equal(subject,j.Subject)
            Assert.Equal(wf.InitialState,j.CurrentState)
            Assert.Equal(newJourneyId,j.JourneyId)
            Assert.Equal(wf.VersionedWorkflowId,j.Workflow)))
    |> Result.injectError(fun e -> raise e)
    |> ignore

[<Fact>]
let ``create journey fails when lookup fails`` () =
    fun () ->
        testWorkflow
        |> Result.bind(fun wf -> 
            let createCmd = { 
                JourneyId = newJourneyId
                VersionedWorkflowId = { wf.VersionedWorkflowId with Version = Workflow.Version 77777UL }
                Subject = subject }
            createJourney lookup createCmd NonExistingJourney)
        |> Result.injectError(fun e -> raise e)
        |> ignore
    |> expectExn<Workflow.UndefinedVersionException>


let testJourney =
    testWorkflow
    |> Result.bind(fun wf ->
        createTest testWorkflow
        |> Result.map(List.fold (evolve wf) NonExistingJourney))

let handleEvolve command state =
    testWorkflow
    |> Result.bind(fun workflow ->
        state
        |> handle lookup command
        |> Result.map (List.fold (evolve workflow) state))

let processCmd command = handleEvolve command |> Result.bind


[<Fact>]
let ``transition`` () =
    testJourney
    |> processCmd (Transition { JourneyId = newJourneyId; TransitionId = 0u })
    |> Result.bind(fun j ->
        match j with
          | NonExistingJourney -> "Journey doesn't exist" |> exn |> Error
          | ActiveJourney _ -> "Journey is Active" |> exn |> Error
          | TerminatedJourney t -> t |> Ok)
    |> Result.map(fun jm ->
        Assert.Equal(1u,jm.CurrentState.Id)
        Assert.True(jm.CurrentState.IsTerminal))
    |> Result.injectError(fun e -> raise e)
    |> ignore

[<Fact>]
let ``invalid transition that doesn't exist`` () =
    fun () ->
        testJourney
        |> processCmd (Transition { JourneyId = newJourneyId; TransitionId = 57463u })
        |> Result.injectError(fun e -> raise e)
        |> ignore
    |> expectExn<Workflow.UndefinedTransitionException>

[<Fact>]
let ``invalid transition`` () =
    fun () ->
        testJourney
        |> processCmd (Transition { JourneyId = newJourneyId; TransitionId = 1u })
        |> Result.injectError(fun e -> raise e)
        |> ignore
    |> expectExn<InvalidTransitionException>

[<Fact>]
let ``reactivate`` () =
    testJourney
    |> processCmd (Transition { JourneyId = newJourneyId; TransitionId = 0u })
    |> processCmd (ReActivate { JourneyId = newJourneyId; TransitionId = 1u })
    |> Result.bind(fun j ->
        match j with
          | NonExistingJourney -> "Journey doesn't exist" |> exn |> Error
          | ActiveJourney a -> a |> Ok 
          | TerminatedJourney _ -> "Journey is Terminated" |> exn |> Error)
    |> Result.map(fun jm ->
        Assert.Equal(0u,jm.CurrentState.Id)
        Assert.False(jm.CurrentState.IsTerminal))
    |> Result.injectError(fun e -> raise e)
    |> ignore

[<Fact>]
let ``reactivate when active`` () =
    fun () ->
        testJourney
        |> processCmd (ReActivate { JourneyId = newJourneyId; TransitionId = 1u })
        |> Result.injectError(fun e -> raise e)
        |> ignore
    |> expectExn<InvalidTransitionException>
