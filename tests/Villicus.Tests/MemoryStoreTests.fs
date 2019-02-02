module MemoryStoreTests

open Xunit
open Villicus.Persistence
open TestUtil
open System

type TestId = TestId of System.Guid

type TestEvent =
  | Turtle
  | Complex of Complex
  | Simple of int
and Complex = {
    A:System.Guid
    Hare: Map<int,string> }

let testStore = MemoryStore.create<TestId,TestEvent> ()
let getTestId () = System.Guid.NewGuid () |> TestId

[<Fact>]
let ``unsaved stream yields empty event list`` () =
    testStore.StreamReader (System.Guid.Empty |> TestId) 0L 1000
    |> Async.map(fun (readList,version,nextVersion) ->
            Assert.True(List.isEmpty readList)
            Assert.Equal(None,nextVersion)
            Assert.Equal(0L,version))

[<Fact>]
let ``version mismatch`` () =
    let testId = getTestId ()
    async {
        let! result = [ Turtle ] |> Seq.ofList |> testStore.StreamWriter testId 1L
        fun () -> result |> Result.mapError(fun e -> raise e) |> ignore
        |> expectExn<VersionMisMatchException> }

[<Fact>]
let ``write, append, and read back events`` () =
    let testId = getTestId ()
    let expectedVersion = 0L
    let inputEvents =
        [ Turtle
          Complex { A = System.Guid.NewGuid (); Hare = Map.empty<int,string> }
          Simple 573 ]
    async {
        match! inputEvents |> Seq.ofList |> testStore.StreamWriter testId expectedVersion with
        | Error e -> raise e
        | Ok () ->
          let! readList,version,nextVersion = testStore.StreamReader testId 0L 1000
          Assert.Equal(3,List.length readList)
          Assert.Equal(None,nextVersion)
          Assert.Equal(3L,version)
          readList |> List.zip inputEvents
          |> List.iter (fun (a,b) -> Assert.Equal(a,b))
          let appendEvents = [ Turtle; Turtle; Turtle ]
          match! appendEvents |> Seq.ofList |> testStore.StreamWriter testId 3L with
          | Error e -> raise e
          | Ok () ->
            // only read back the three events we just appended
            let! endList,endVersion,endNextVersion = testStore.StreamReader testId 4L 1000
            Assert.Equal(3,List.length endList)
            Assert.Equal(None,endNextVersion)
            Assert.Equal(6L,endVersion)
            endList |> List.zip appendEvents
            |> List.iter (fun (a,b) -> Assert.Equal(a,b))
            // now read back all events
            let expectedAllEvents = inputEvents @ appendEvents
            let! allList,allVersion,allNextVersion = testStore.StreamReader testId 0L 1000
            Assert.Equal(6,List.length allList)
            Assert.Equal(None,allNextVersion)
            Assert.Equal(6L,allVersion)
            allList |> List.zip expectedAllEvents
            |> List.iter (fun (a,b) -> Assert.Equal(a,b)) }

[<Fact>]
let ``read list larger than buffer`` () =
    raise (NotImplementedException())

[<Fact>]
let ``write and read multiple streams at once`` () =
    raise (NotImplementedException())

[<Fact>]
let ``dispatcher tests`` () =
    raise (NotImplementedException())
