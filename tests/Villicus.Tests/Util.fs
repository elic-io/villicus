module TestUtil

//TODO: Look into using Assert.Raises instead?
let inline expectExn<'e when 'e :> exn> (f:unit->unit) =
    try
        f ()
    with 
      :? 'e -> ()
      | e -> raise e

//let inline expectError (f:unit->Result<'a,'b>) (x:'b) =
//    let y = f ()
//    Xunit.Assert.Equal(x,y)

let inline raiseIfError e =
    match e with
    | Error _ -> raise (e.ToString() |> exn)
    | Ok _ -> ()


[<Xunit.Fact>]
let ``Expect Exception Works`` () =
    expectExn<System.Exception> <| fun () -> failwith "generic Exception"

