namespace Villicus.Persistence

type VersionMisMatchException (expected,actual) =
    inherit exn(sprintf "version %O expected, recieved %O" expected actual)
  with
    member __.Expected = expected
    member __.Actual = actual

//type StreamReader<'key,'event> = 'key -> int64 -> int -> Async<'event list * int64 * int64 option>
//type StreamWriter<'key,'event> = 'key -> int64 -> seq<'event> -> Async<Result<unit,exn>>
//type DataStore<'key,'event> = {
//    StreamReader: StreamReader<'key,'event>
//    StreamWriter: StreamWriter<'key,'event> }

type DataStore<'key,'event> = {
    StreamReader: 'key -> int64 -> int -> Async<'event list * int64 * int64 option>
    StreamWriter: 'key -> int64 -> seq<'event> -> Async<Result<unit,exn>> }
