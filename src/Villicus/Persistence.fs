namespace Villicus.Persistence

type VersionMisMatchException (expected,actual) =
    inherit exn(sprintf "version %O expected, recieved %O" expected actual)
  with
    member __.Expected = expected
    member __.Actual = actual

type StreamDataStore<'key,'event> = {
    ReadStream: 'key -> int64 -> int -> Async<'event list * int64 * int64 option>
    AppendToStream: 'key -> int64 -> seq<'event> -> Async<Result<unit,exn>> }

type Repository<'key,'value when 'key : comparison> = {
    Retrieve: 'key -> Async<'value option>

    //TODO: this should be an IObservable to work with Async correctly.
    // RetrieveAll: Async<'value seq>
    RetrieveItems: ('key -> 'value -> bool) -> Async<Map<'key,'value>>
    
    Save: 'key -> 'value -> Async<Result<unit,exn>> }
