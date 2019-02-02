module CommandHandlerTests

open System
open Xunit
open Villicus.Domain
open TestUtil
open Villicus
open Villicus.Persistence
open Villicus.CommandHandlers

type WorkflowAgent = Agent<WFCommand<WorkflowCommand,WorkflowEvent,int64*Workflow,exn>>

type AgentTestFixture = {
    CTS: System.Threading.CancellationTokenSource
    Observable: IObservable<WorkflowEvent>
    WorkflowId: WorkflowId
    Agent: WorkflowAgent }

let createAgentFixture () =
    let guid = System.Guid.NewGuid ()
    let dataStore = MemoryStore.create<string,WorkflowEvent> ()
    let singleCts = new System.Threading.CancellationTokenSource ()
    let observableSA,broadcastSA = 
        Observable.createObservableAgent<WorkflowEvent> 0 singleCts.Token
    { CTS = singleCts
      Observable = observableSA
      WorkflowId = WorkflowId guid
      Agent = Process.start dataStore.StreamReader dataStore.StreamWriter broadcastSA guid }

let processResult checkModel = function
    | Ok (_,wf) -> 
    wf |> (Option.inject checkModel)
    |> ignore
    | Error e -> raise e

let procCmd r cmd : WFCommand<WorkflowCommand,WorkflowEvent,int64*Workflow,exn> = 
    cmd |> ProcessCommand<WorkflowCommand,int64*Workflow,exn>.New r |> ProcCommand

let postAndReply (agent:WorkflowAgent) cmd = agent.PostAndReply(fun r -> procCmd r cmd)

[<Fact>]
let ``agent workflow creation`` () =
    let testWorkFlowName = "TestAgentWorkflow"
    let af = createAgentFixture ()
    CreateWorkflowCommand (af.WorkflowId, testWorkFlowName) |> CreateWorkflow
    |> postAndReply af.Agent
    |> processResult (fun wfm -> Assert.Equal(testWorkFlowName,wfm.Name))
    
    af.CTS.Cancel() // stops sending events to observers

[<Fact>]
let ``agent get workflow state`` () =
    let testWorkFlowName = "TestAgentWorkflow"
    let af = createAgentFixture ()
    CreateWorkflowCommand (af.WorkflowId, testWorkFlowName) |> CreateWorkflow
    |> postAndReply af.Agent |> ignore
    af.Agent.PostAndReply(fun r -> GetState (af.WorkflowId,0L,r))
    |> processResult (fun wfm -> Assert.Equal(testWorkFlowName,wfm.Name))
    
    af.CTS.Cancel() // stops sending events to observers

[<Fact>]
let ``agent get workflow events`` () =
    let testWorkFlowName = "TestAgentWorkflow"
    let af = createAgentFixture ()
    CreateWorkflowCommand (af.WorkflowId, testWorkFlowName) |> CreateWorkflow
    |> postAndReply af.Agent |> ignore
    af.Agent.PostAndReply(fun r -> ReadStream.New<WorkflowEvent,exn> af.WorkflowId 0L 2 r |> ReadStream)
    |> Result.bind(fun (eventList,lastEventId,nextEventId) ->
        Assert.Equal(2,List.length eventList)
        Assert.Equal(2L,lastEventId)
        Assert.Equal(Some 3L,nextEventId)
        af.Agent.PostAndReply(fun r -> ReadStream.New<WorkflowEvent,exn> af.WorkflowId nextEventId.Value 2 r |> ReadStream))
    |> Result.map(fun (eventList,lastEventId,nextEventId) ->
        Assert.Equal(2,List.length eventList)
        Assert.Equal(4L,lastEventId)
        Assert.Equal(None,nextEventId))
    |> Result.injectError(fun e -> raise e)
    |> ignore
    af.CTS.Cancel() // stops sending events to observers
