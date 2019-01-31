﻿namespace Villicus.Persistence

open System.Collections.Concurrent

module MemoryStore =
    
    /// This will go through the full eventList every time it's called
    let readStream (store:ConcurrentDictionary<'a,'b list>) streamId version count =
        let rec skip64 i64 li =
            let maxInt = System.Int32.MaxValue |> int64
            if i64 < maxInt then
                List.skip (int i64) li
            else
                List.skip System.Int32.MaxValue li
                |> skip64 i64
        let takeUpTo mx = match mx with | 0 -> id | _ -> List.takeWhile(fun (i,_) -> i < mx)
        match store.TryGetValue streamId with
        | false, _ -> (List.empty<'b>, 0L, None) |> Async.result
        | true, value ->
            //|> skip64 version
            let stream = value |> List.indexed |> List.skip (int version)
            let iEvents = stream |> takeUpTo count
            let events = iEvents |> List.map snd
            let lastEventNumber = iEvents |> List.last |> fst |> (+) 1
            let nextEventNumber =
                let nextEventCandidate = stream |> takeUpTo (count+1) |> List.last |> fst |> (+) 1
                match lastEventNumber = nextEventCandidate with
                    | true -> None
                    | false -> nextEventCandidate |> int64 |> Some
            (events, int64 lastEventNumber, nextEventNumber) |> Async.result

    let appendToStream (store: ConcurrentDictionary<'a,'b list>) streamId (expectedVersion:int64) newEvents = 
        // AddOrUpdate returns the same type as input, so we have to raise the exception to
        // then catch it and pass the error type back out
        // unless we wanted to store Result type, which we don't
        let addValueFactory _ =
            if expectedVersion = 0L then Seq.toList newEvents
            else raise (VersionMisMatchException (expectedVersion,0L))
        let updateValueFactory _ stream =
            let version = Seq.length stream |> int64
            if version = expectedVersion then
                stream @ (Seq.toList newEvents)
            else raise (VersionMisMatchException (expectedVersion,version))
        try
            match Seq.isEmpty newEvents with
            | true -> () // means we don't add or append for empty events
            | false -> store.AddOrUpdate(streamId, addValueFactory, updateValueFactory) |> ignore
            |> Ok
        with
            | e -> Error e
        |> Async.result

    let create<'a,'b> () = 
        let store = ConcurrentDictionary<'a,'b list> ()
        { StreamReader = readStream store
          StreamWriter = appendToStream store }