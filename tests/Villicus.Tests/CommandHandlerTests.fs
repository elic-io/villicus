module CommandHandlerTests

open System
open Xunit
open Villicus.Domain
open TestUtil
open Villicus
open Villicus.Persistence
open Villicus.CommandHandlers

type WorkflowAgent = Agent<WFCommand<WorkflowCommand,WorkflowEvent,WorkflowId,int64*Workflow,exn>>

type AgentTestFixture = {
    CTS: System.Threading.CancellationTokenSource
    Observable: IObservable<WorkflowEvent>
    WorkflowId: WorkflowId
    Agent: WorkflowAgent }

let createAgentFixture () =
    let guid = System.Guid.NewGuid ()
    let dataStore = MemoryEventStore.create<string,WorkflowEvent> ()
    let singleCts = new System.Threading.CancellationTokenSource ()
    let observableSA,broadcastSA = 
        Observable.createObservableAgent<WorkflowEvent> 0 singleCts.Token
    { CTS = singleCts
      Observable = observableSA
      WorkflowId = WorkflowId guid
      Agent = Workflow.start dataStore broadcastSA guid }

let processResult checkModel = function
    | Ok (_,wf) -> 
        wf |> (Option.inject checkModel)
        |> ignore
    | Error _ -> ()

let inline postAndReply (agent:WorkflowAgent) =
    (Result.mapError CommandCreationError.ToExn)
    >> (Result.map WFCommand.newCmd)
    >> (Result.bind agent.PostAndReply) >> Result.injectError raise

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
    let dispatcher = MemoryEventStore.create<string,WorkflowEvent> () |> Workflow.createDispatcher 0
    "dispatcher workflow creation"
    |> workflowCreation dispatcher.Agent dispatcher.CancellationTokenSource (System.Guid.NewGuid() |> WorkflowId)



let getWorkflowState agent (cts:System.Threading.CancellationTokenSource) workflowId testWFname =
    CommandAPI.createWorkflow workflowId testWFname
    |> postAndReply agent |> ignore
    agent.PostAndReply(fun r -> GetState (workflowId,0L,r))
    |> processResult (fun wfm -> Assert.Equal(testWFname,wfm.Name))
    cts.Cancel() // stops sending events to observers


[<Fact>]
let ``agent get workflow state`` () =
    let af = createAgentFixture ()
    "agent get workflow state"
    |> getWorkflowState af.Agent af.CTS af.WorkflowId

[<Fact>]
let ``dispatcher get workflow state`` () =
    let dispatcher = MemoryEventStore.create<string,WorkflowEvent> () |> Workflow.createDispatcher 0
    "dispatcher get workflow state"
    |> getWorkflowState dispatcher.Agent dispatcher.CancellationTokenSource (System.Guid.NewGuid() |> WorkflowId)


let getWorkflowEvents agent (cts:System.Threading.CancellationTokenSource) workflowId testWFname =
    CommandAPI.createWorkflow workflowId testWFname
    |> postAndReply agent |> ignore
    agent.PostAndReply(fun r -> 
        ReadStream.New<WorkflowEvent> workflowId 0L 2 r |> ReadStream)
    |> Result.bind(fun (eventList,lastEventId,nextEventId) ->
        Assert.Equal(2,List.length eventList)
        Assert.Equal(2L,lastEventId)
        Assert.Equal(Some 3L,nextEventId)
        agent.PostAndReply(fun r -> 
            ReadStream.New<WorkflowEvent> workflowId nextEventId.Value 2 r |> ReadStream))
    |> Result.map(fun (eventList,lastEventId,nextEventId) ->
        Assert.Equal(2,List.length eventList)
        Assert.Equal(4L,lastEventId)
        Assert.Equal(None,nextEventId))
    |> Result.injectError(fun e -> raise e)
    |> ignore
    cts.Cancel() // stops sending events to observers

[<Fact>]
let ``agent get workflow events`` () =
    let af = createAgentFixture ()
    "agent get workflow events"
    |> getWorkflowEvents af.Agent af.CTS af.WorkflowId

[<Fact>]
let ``dispatcher get workflow events`` () =
    let dispatcher = MemoryEventStore.create<string,WorkflowEvent> () |> Workflow.createDispatcher 0
    "dispatcher get workflow events"
    |> getWorkflowEvents dispatcher.Agent dispatcher.CancellationTokenSource (System.Guid.NewGuid() |> WorkflowId)







type JourneyAgent<'a> = Agent<ResultCommand<JourneyCommand<'a>,int64*Journey<'a>,exn>>

type JourneyTestFixture<'a> = {
    CTS: System.Threading.CancellationTokenSource
    Observable: IObservable<JourneyEvent<'a>>
    JourneyId: JourneyId
    Workflow: Published<WorkflowModel>
    Agent: JourneyAgent<'a> }

let createJourneyFixture<'a> workflow =
    let guid = System.Guid.NewGuid ()
    let dataStore = MemoryEventStore.create<string,JourneyEvent<'a>> ()
    let singleCts = new System.Threading.CancellationTokenSource ()
    let observableSA,broadcastSA = 
        Observable.createObservableAgent<JourneyEvent<'a>> 0 singleCts.Token
    { CTS = singleCts
      Observable = observableSA
      JourneyId = JourneyId guid
      Workflow = workflow
      Agent = Journey.start dataStore (fun () -> workflow) broadcastSA guid }

let postAndReplyJ<'a> (agent:JourneyAgent<'a>) = 
    ResultCommand.newCmd >> agent.PostAndReply >> Result.injectError raise

let processResultJ (checkModel:JourneyModel<'a> -> unit) = function
    | Ok (_,j) -> 
        match j with
        | ActiveJourney jm | TerminatedJourney jm -> checkModel jm
        | NonExistingJourney _ -> raise (exn "journey does not exist")
    | Error e -> raise e

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
    af.Agent.PostAndReply(fun r -> GetState (af.WorkflowId,0L,r))
    |> processResult (fun wfm -> 
        let jf = wfm |> Published |> createJourneyFixture<System.Guid>
        System.Guid.NewGuid () 
        |> journeyCreation jf.Agent jf.CTS jf.JourneyId wfm.VersionedWorkflowId)
    |> ignore

[<Fact>]
let ``dispatcher journey creation`` () =
    let repo = MemoryRepository.create<VersionedWorkflowId,Published<WorkflowModel>> ()
    let wfName = "workflow for agent journey creation"
    let workflowId = System.Guid.NewGuid () |> WorkflowId
    let journeyId = System.Guid.NewGuid () |> JourneyId
    let wfDispatcher = 
        let d = MemoryEventStore.create<string,WorkflowEvent> () |> Workflow.createDispatcher 0
        Workflow.versionProjection d repo.Save
        d
    let jDispatcher =
        let es = MemoryEventStore.create<string,JourneyEvent<System.Guid>> ()
        Journey.createDispatcher 0 es repo
    // we add an second observer to trigger the journey creation after the publish
    wfDispatcher.Observable
    |> Observable.add(function
        | WorkflowPublished e -> 
            wfDispatcher.Agent.PostAndReply(fun r -> GetVersionedState (e,r))
            |> Result.map(snd >> Option.map(fun wfm ->
                System.Guid.NewGuid () //subject
                |> journeyCreation jDispatcher.Agent jDispatcher.CancellationTokenSource journeyId wfm.VersionedWorkflowId))
            |> ignore
        | _ -> ())

    CommandAPI.createWorkflow workflowId wfName
    |> postAndReply wfDispatcher.Agent 
    |> Result.injectError(fun e -> raise e)
    |> ignore
    workflowId |> CommandAPI.publishWorkflow
    |> postAndReply wfDispatcher.Agent
    |> Result.injectError(fun e -> raise e)
    |> ignore

