module MemoryRepositoryTests

open Villicus.Persistence
open Villicus.CommandHandlers
open Villicus.Domain
open Xunit

[<Fact>]
let ``project versions from dispatcher`` () =
    let dispatcher = MemoryEventStore.create () |> Workflow.createDispatcher 0
    let repo = MemoryRepository.create<VersionedWorkflowId,Published<WorkflowModel>> ()
    Workflow.versionProjection dispatcher repo.Save
