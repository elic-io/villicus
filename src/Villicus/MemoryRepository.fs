namespace  Villicus.Persistence

module MemoryRepository =
    open System.Collections.Concurrent

    /// MemoryStore.StreamReader will internally go through the full eventList every time it's called
    /// It's inefficient
    /// It also only supports int32 event addresses
    /// But that's OK it's really just a mock/sample implementation for testing and development
    let private readItem (store:ConcurrentDictionary<'a,'b>) itemId =
        match store.TryGetValue itemId with
        | false, _ -> None 
        | true, value -> value |> Some
        |> Async.result

    let private addOrUpdateItem (store: ConcurrentDictionary<'a,'b>) itemId newValue =
        // AddOrUpdate returns the same type as input, so we have to raise the exception to
        // then catch it and pass the error type back out
        // unless we wanted to store Result type, which we don't
        try
            store.AddOrUpdate(itemId,(fun _ -> newValue),(fun _ _ -> newValue)) |> ignore
            |> Ok
        with
            | e -> Error e
        |> Async.result

    let private retrieveItems (store: ConcurrentDictionary<'a,'b>) (filter: 'a -> 'b -> bool) = async {
        let filterConv (kvp:System.Collections.Generic.KeyValuePair<'a,'b>) =
            filter kvp.Key kvp.Value
        return
            Seq.filter filterConv store
            |> Seq.map (fun kvp -> (kvp.Key, kvp.Value))
            |> Map.ofSeq }
    
    let create<'a,'b when 'a : comparison> () =
        let store = ConcurrentDictionary<'a,'b> ()
        { Retrieve = readItem store
          RetrieveItems = retrieveItems store
          Save = addOrUpdateItem store }
