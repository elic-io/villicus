module CommandHandlerTests

open System
open Xunit
open Villicus.Domain
open TestUtil
open Villicus
open Villicus.Persistence
open Villicus.CommandHandlers

type AgentTestFixture = {
    //DataStore: DataStore<string,WorkflowEvent>
    CTS: System.Threading.CancellationTokenSource
    Observable: IObservable<WorkflowEvent>
    //Broadcast: Result<WorkflowEvent,exn> -> unit
    WorkflowId: WorkflowId
    Agent: Agent<ProcessCommand<WorkflowCommand,Workflow,exn>> }

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
    | Ok wf -> 
        wf |> (Option.inject checkModel)
        |> ignore |> Async.result
    | Error e -> raise e
let wrapCommand checkFunc command =
    command
    |> ProcessCommand<WorkflowCommand,Workflow,exn>.New (processResult checkFunc)


[<Fact>]
let `` agent workflow creation`` () =
    let testWorkFlowName = "TestAgentWorkflow"
    let singleAgent = createAgentFixture ()
    CreateWorkflowCommand (singleAgent.WorkflowId, testWorkFlowName) |> CreateWorkflow
    |> wrapCommand (fun wfm -> Assert.Equal(testWorkFlowName,wfm.Name))
    |> singleAgent.Agent.Post
    |> ignore
    
    singleAgent.CTS.Cancel() // stops sending events
