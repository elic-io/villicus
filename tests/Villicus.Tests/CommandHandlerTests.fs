module CommandHandlerTests

open System
open Xunit
open Villicus.Domain
open TestUtil
open Sentinam.Persistence
open Sentinam.Persistence.Memory
open Villicus
open Villicus.CommandHandlers

type WorkflowAgent = Sentinam.Agent<Workflow.Envelope<uint64>>

type AgentTestFixture = {
    CTS: System.Threading.CancellationTokenSource
    Observable: IObservable<WorkflowEvent>
    WorkflowId: WorkflowId
    DataStore: StreamDataStore<string,WorkflowEvent,uint64>
    Agent: WorkflowAgent }

let createAgentFixture () =
    let worklfowId = System.Guid.NewGuid () |> WorkflowId
    let dataStore = MemoryEventStore.create<string,WorkflowEvent,uint64> ()
    let singleCts = new System.Threading.CancellationTokenSource ()
    let observableSA,broadcastSA =
        Observable.createObservableAgent<WorkflowEvent> 0 singleCts.Token
    { CTS = singleCts
      Observable = observableSA
      WorkflowId = worklfowId
      DataStore = dataStore
      Agent = Workflow.start dataStore broadcastSA singleCts.Token worklfowId }

let processResult checkModel = function
    | Ok (_,wf) -> 
        wf |> (Option.inject checkModel)
        |> ignore
    | Error _ -> ()

let inline postAndReply (agent:WorkflowAgent) =
    (Result.mapError CommandCreation)
    >> (Result.map Sentinam.Envelope.newCmd)
    >> (Result.bind agent.PostAndReply) // >> Result.injectError raise

let workflowCreation agent (cts:System.Threading.CancellationTokenSource) workflowId testWFname =
    CommandAPI.createWorkflow workflowId testWFname
    |> postAndReply agent
    |> processResult (fun wfm -> Assert.Equal(testWFname,wfm.Name))
    cts.Cancel() // stops sending events to observers

[<Fact>]
let ``agent workflow creation`` () =
    let af = createAgentFixture ()
    workflowCreation af.Agent af.CTS af.WorkflowId "agent workflow creation"

[<Fact>]
let ``dispatcher workflow creation`` () =
    let af = createAgentFixture ()
    let dispatcher = MemoryEventStore.create<string,WorkflowEvent,uint64> () |> Workflow.createDispatcherAgent (fun _ -> ()) af.CTS.Token
    "dispatcher workflow creation"
    |> workflowCreation dispatcher af.CTS (System.Guid.NewGuid() |> WorkflowId)



let getWorkflowState agent (cts:System.Threading.CancellationTokenSource) workflowId testWFname =
    CommandAPI.createWorkflow workflowId testWFname
    |> postAndReply agent |> ignore
    agent.PostAndReply(fun r -> Sentinam.Envelope.newGetState workflowId 0UL r)
    |> processResult (fun wfm -> Assert.Equal(testWFname,wfm.Name))
    cts.Cancel() // stops sending events to observers


[<Fact>]
let ``agent get workflow state`` () =
    let af = createAgentFixture ()
    "agent get workflow state"
    |> getWorkflowState af.Agent af.CTS af.WorkflowId

[<Fact>]
let ``dispatcher get workflow state`` () =
    let af = createAgentFixture ()
    let dispatcher = MemoryEventStore.create<string,WorkflowEvent,uint64> () |> Workflow.createDispatcherAgent (fun _ -> ()) af.CTS.Token
    "dispatcher get workflow state"
    |> getWorkflowState dispatcher af.CTS (System.Guid.NewGuid() |> WorkflowId)


let getWorkflowEvents agent (cts:System.Threading.CancellationTokenSource) workflowId testWFname =
    CommandAPI.createWorkflow workflowId testWFname
    |> postAndReply agent |> ignore
    agent.PostAndReply(fun r -> 
        Sentinam.Envelope.newReadStream workflowId 0UL 2 r)
    |> Result.bind(fun (eventList,lastEventId,nextEventId) ->
        Assert.Equal(2,Seq.length eventList)
        Assert.Equal(2UL,lastEventId)
        Assert.Equal(Some 3UL,nextEventId)
        agent.PostAndReply(fun r -> 
            Sentinam.Envelope.newReadStream workflowId nextEventId.Value 2 r))
    |> Result.map(fun (eventList,lastEventId,nextEventId) ->
        Assert.Equal(2,Seq.length eventList)
        Assert.Equal(4UL,lastEventId)
        Assert.Equal(None,nextEventId))
    |> raiseIfError
    cts.Cancel() // stops sending events to observers

[<Fact>]
let ``agent get workflow events`` () =
    let af = createAgentFixture ()
    "agent get workflow events"
    |> getWorkflowEvents af.Agent af.CTS af.WorkflowId

[<Fact>]
let ``dispatcher get workflow events`` () =
    let af = createAgentFixture ()
    let dispatcher = MemoryEventStore.create<string,WorkflowEvent,uint64> () |> Workflow.createDispatcherAgent (fun _ -> ()) af.CTS.Token
    "dispatcher get workflow events"
    |> getWorkflowEvents dispatcher af.CTS (System.Guid.NewGuid () |> WorkflowId)







type JourneyAgent<'a> = Sentinam.Agent<Journey.Envelope<'a,uint64>>

type JourneyTestFixture<'a> = {
    CTS: System.Threading.CancellationTokenSource
    Observable: IObservable<JourneyEvent<'a>>
    JourneyId: JourneyId
    Workflow: Published<WorkflowModel>
    Agent: JourneyAgent<'a> }

let createJourneyFixture<'a> workflow =
    let journeyId = System.Guid.NewGuid () |> JourneyId
    let dataStore = MemoryEventStore.create<string,JourneyEvent<'a>,uint64> ()
    let singleCts = new System.Threading.CancellationTokenSource ()
    let observableSA,broadcastSA = 
        Observable.createObservableAgent<JourneyEvent<'a>> 0 singleCts.Token
    { CTS = singleCts
      Observable = observableSA
      JourneyId = journeyId
      Workflow = workflow
      Agent = Journey.start (fun () -> workflow) dataStore broadcastSA singleCts.Token journeyId }

let postAndReplyJ<'a> (agent:JourneyAgent<'a>) = 
    Sentinam.Envelope.newCmd >> agent.PostAndReply // >> Result.injectError raise

let processResultJ (checkModel:JourneyModel<'a> -> unit) = function
    | Ok (_,j) -> 
        match j with
        | ActiveJourney jm | TerminatedJourney jm -> checkModel jm
        | NonExistingJourney _ -> raise (exn "journey does not exist")
    | Error e -> e.ToString() |> exn |> raise

let journeyCreation agent (cts:System.Threading.CancellationTokenSource) journeyId versionedWorkflowId subject =
    { JourneyId = journeyId; VersionedWorkflowId = versionedWorkflowId; Subject = subject } |> CreateJourney
    |> postAndReplyJ agent
    |> processResultJ (fun jm -> Assert.Equal(0u,jm.CurrentState.Id))
    cts.Cancel() // stops sending events to observers

[<Fact>]
let ``agent journey creation`` () =
    let wfName = "workflow for agent journey creation"
    let af = createAgentFixture ()
    CommandAPI.createWorkflow af.WorkflowId wfName
    |> postAndReply af.Agent |> ignore
    af.Agent.PostAndReply(fun r -> Sentinam.Envelope.newGetState af.WorkflowId 0UL r)
    |> processResult (fun wfm -> 
        let jf = wfm |> Published |> createJourneyFixture<System.Guid>
        System.Guid.NewGuid () 
        |> journeyCreation jf.Agent jf.CTS jf.JourneyId wfm.VersionedWorkflowId)
    |> ignore

[<Fact>]
let ``dispatcher journey creation`` () =
    let repo = Sentinam.Persistence.Memory.MemoryRepository.create<VersionedWorkflowId,Published<WorkflowModel>> ()
    let wfName = "workflow for agent journey creation"
    let af = createAgentFixture ()
    let workflowId = System.Guid.NewGuid () |> WorkflowId
    let journeyId = System.Guid.NewGuid () |> JourneyId
    af.Observable.Add (Workflow.versionProjection af.Agent repo.Save)
    let wfDispatcher = af.Agent
    let jDispatcher =
        let es = MemoryEventStore.create<string,JourneyEvent<System.Guid>,uint64> ()
        Journey.createDispatcherAgent es af.CTS.Token (fun _ -> ()) repo
    // we add an second observer to trigger the journey creation after the publish
    af.Observable
    |> Observable.add(function
        | WorkflowPublished e ->
            let (pulishedWorkflowId, publishedWorkflowVersion) = VersionedWorkflowId.Decompose e
            wfDispatcher.PostAndReply(fun r -> Sentinam.Envelope.newGetState pulishedWorkflowId publishedWorkflowVersion r)
            |> Result.map(snd >> Option.map(fun wfm ->
                System.Guid.NewGuid () //subject
                |> journeyCreation jDispatcher af.CTS journeyId wfm.VersionedWorkflowId))
            |> ignore
        | _ -> ())

    CommandAPI.createWorkflow workflowId wfName
    |> postAndReply wfDispatcher
    |> raiseIfError
    workflowId |> CommandAPI.publishWorkflow
    |> postAndReply wfDispatcher
    |> raiseIfError
