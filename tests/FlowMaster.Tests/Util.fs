module TestUtil

//TODO: Look into using Assert.Raises instead?
let inline expectExn<'e when 'e :> exn> (f:unit->unit) =
    try
        f ()
    with 
      :? 'e -> ()
      | e -> raise e

[<Xunit.Fact>]
let ``Expect Exception Works`` () =
    expectExn<System.Exception> <| fun () -> failwith "generic Exception"

