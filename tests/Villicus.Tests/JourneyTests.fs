module JourneyTests

open System
open Xunit
open Villicus.Domain
open TestUtil

let subject = Guid.NewGuid()
let newJourneyId = Guid.NewGuid() |> JourneyId
let testWorkflow =
    WorkflowTests.testWorkflow
    |> WorkflowTests.processCmd (CommandAPI.addTransition WorkflowTests.testWorkflowId "Reactivate" 1u 0u)
    |> Result.bind(Result.ofOption NonExistant)
    |> Result.mapError (fun _ -> "This shouldn't ever happen" |> exn |> JourneyError.unknown newJourneyId)

let lookup vwid =
    testWorkflow
    |> Result.bind(fun w ->
        let v = w.VersionedWorkflowId
        match vwid = v with
          | true -> Ok w
          | false -> JourneyError.undefinedVersion { Id = v.Id; Version = v.Version } |> Error)

let createTest (workflow:Result<WorkflowModel,JourneyError>) =
    workflow
    |> Result.bind(fun wf -> 
        let createCmd = { 
            JourneyId = newJourneyId
            VersionedWorkflowId = wf.VersionedWorkflowId
            Subject = subject }
        Journey.createJourney lookup createCmd Journey.NewNonExisting)

[<Fact>]
let ``create journey`` () =
    testWorkflow
    |> Result.bind(fun wf -> 
        let createCmd = { 
            JourneyId = newJourneyId
            VersionedWorkflowId = wf.VersionedWorkflowId
            Subject = subject }
        Journey.createJourney lookup createCmd Journey.NewNonExisting
        |> Result.map(List.fold (Journey.evolve wf) Journey.NewNonExisting)
        |> Result.bind (Journey.toResult newJourneyId)
        |> Result.map(fun j -> 
            Assert.Equal(subject,j.Subject)
            Assert.Equal(wf.InitialState,j.CurrentState)
            Assert.Equal(newJourneyId,j.JourneyId)
            Assert.Equal(wf.VersionedWorkflowId,j.Workflow)))
    |> raiseIfError

[<Fact>]
let ``create journey fails when lookup fails`` () =
    testWorkflow
    |> Result.bind(fun wf -> 
        let createCmd = { 
            JourneyId = newJourneyId
            VersionedWorkflowId = { wf.VersionedWorkflowId with Version = Version 77777UL }
            Subject = subject }
        Journey.createJourney lookup createCmd Journey.NewNonExisting)
    |> Result.map2
        (fun _ -> raise (exn "this should have been an duplicate workflowId error"))
        (fun e -> Assert.True(match e with | JourneyError.UndefinedVersion _ -> true | _ -> false))
    |> ignore

let testJourney =
    testWorkflow
    |> Result.bind(fun wf ->
        createTest testWorkflow
        |> Result.map(List.fold (Journey.evolve wf) Journey.NewNonExisting))

let handleEvolve command state =
    testWorkflow
    |> Result.bind(fun workflow ->
        state
        |> Journey.handle lookup command
        |> Result.map (Seq.fold (Journey.evolve workflow) state))

let processCmd command = handleEvolve command |> Result.bind


[<Fact>]
let ``transition`` () =
    testJourney
    |> processCmd (TransitionCommand { JourneyId = newJourneyId; TransitionId = 0u })
    |> Result.bind(fun j ->
        match j with
          | NonExistingJourney _ -> Error JourneyError.NonExistant
          | ActiveJourney _ -> "Journey is Active" |> exn |> JourneyError.unknown newJourneyId |> Error
          | TerminatedJourney t -> t |> Ok)
    |> Result.map(fun jm ->
        Assert.Equal(1u,jm.CurrentState.Id)
        Assert.True(jm.CurrentState.IsTerminal))
    |> raiseIfError

[<Fact>]
let ``invalid transition that doesn't exist`` () =
    testJourney
    |> processCmd (TransitionCommand { JourneyId = newJourneyId; TransitionId = 57463u })
    |> Result.map2
        (fun _ -> raise (exn "this should have been an UndefinedTransition error"))
        (fun e -> Assert.True(match e with | JourneyError.UndefinedTransition _ -> true | _ -> false))
    |> ignore

[<Fact>]
let ``invalid transition`` () =
    testJourney
    |> processCmd (TransitionCommand { JourneyId = newJourneyId; TransitionId = 1u })
    |> Result.map2
        (fun _ -> raise (exn "this should have been an InvalidTransition error"))
        (fun e -> Assert.True(match e with | InvalidTransition _ -> true | _ -> false))
    |> ignore

[<Fact>]
let ``reactivate`` () =
    testJourney
    |> processCmd (TransitionCommand { JourneyId = newJourneyId; TransitionId = 0u })
    |> processCmd (ReActivate { JourneyId = newJourneyId; TransitionId = 1u })
    |> Result.bind(fun j ->
        match j with
          | NonExistingJourney _ -> "Journey doesn't exist" |> exn |> JourneyError.unknown newJourneyId |> Error
          | ActiveJourney a -> a |> Ok 
          | TerminatedJourney _ -> "Journey is Terminated" |> exn |> JourneyError.unknown newJourneyId |> Error)
    |> Result.map(fun jm ->
        Assert.Equal(0u,jm.CurrentState.Id)
        Assert.False(jm.CurrentState.IsTerminal))
    |> raiseIfError

[<Fact>]
let ``reactivate when active`` () =
    testJourney
    |> processCmd (ReActivate { JourneyId = newJourneyId; TransitionId = 1u })
    |> Result.map2
        (fun _ -> raise (exn "this should have been an InvalidTransition error"))
        (fun e -> Assert.True(match e with | InvalidTransition _ -> true | _ -> false))
    |> ignore
