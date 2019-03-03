namespace Villicus

open System


type State<'s, 'a> = State of ('s -> ('a * 's))

module State =
    let inline run state x = let (State(f)) = x in f state
    let get = State(fun s -> s, s)
    let put newState = State(fun _ -> (), newState)
    let map f s = State(fun (state: 's) ->
        let x, state = run state s
        f x, state)

[<AutoOpen>]
module StateBuilder =

    /// The state monad passes around an explicit internal state that can be
    /// updated along the way. It enables the appearance of mutability in a purely
    /// functional context by hiding away the state when used with its proper operators
    /// (in StateBuilder()). In other words, you implicitly pass around an implicit
    /// state that gets transformed along its journey through pipelined code.
    type StateBuilder<'s,'a> (save: State<'s,'a> -> State<'s,'a>, retrieve: unit -> Option<State<'s,'a>> ) =
        member this.Zero () = State(fun s -> (), s)
        member this.Return x = State(fun s -> x, s) |> save
        member inline this.ReturnFrom (x: State<'s, 'a>) = save x
        member this.Bind (x, f) : State<'s, 'b> =
            State(fun state ->
                let (result: 'a), state =
                    retrieve ()
                    |> Option.defaultValue x
                    |> State.run state
                State.run state (f result))
        member this.Combine (x1: State<'s, 'a>, x2: State<'s, 'b>) =
            State(fun state ->
                let result, state = State.run state x1
                State.run state x2)
        member this.Delay f : State<'s, 'a> = f ()
        member this.For (seq, (f: 'a -> State<'s, 'b>)) =
            seq
            |> Seq.map f
            |> Seq.reduceBack (fun x1 x2 -> this.Combine (x1, x2))
        member this.While (f, x) =
            if f () then this.Combine (x, this.While (f, x))
            else this.Zero ()


    type Accumulator =
        | Initial
        | FirstStepStarted of InProgress<int> // long running off-loaded task
        | FirstStepComplete of int
        | SecondStepStarted of int * InProgress<string>  // long running off-loaded task
        | SecondStepComplete of int * string
        | ThirdStepComplete of Result<Guid,string> // short task completes inline
        | Expired of stepName:string
    and InProgress<'a> = { WhenLaunched: DateTime; TimeoutLimit:TimeSpan; CheckResult: unit -> 'a option}
      with
        member x.TimedOut utcNow = utcNow - x.WhenLaunched > x.TimeoutLimit
        static member New wl tol cr = { WhenLaunched = wl; TimeoutLimit = tol; CheckResult = cr }
    
    let state = StateBuilder<Accumulator,Accumulator> (id, fun () -> None)

module AJourney =

    let step1 (timeout:TimeSpan) = state {
        let! currentState = State.get
        match currentState with
        | Initial -> 
            // code here to start first step
            let checkProgress () = Some 1
            return  InProgress<int>.New DateTime.UtcNow timeout checkProgress |> FirstStepStarted
        | FirstStepStarted proc ->
            match (proc.TimedOut DateTime.UtcNow, proc.CheckResult ()) with
            | (true,_) -> return Expired "step1"
            | (_, Some result) -> return FirstStepComplete result
            | (_, None) -> return currentState // keep waiting
        | _ -> return currentState
    }

    let step2 timeout = state {
        let s2 = "test"
        let! currentState = State.get
        match currentState with
        | FirstStepComplete st1 -> 
            // code here to start second step
            let checkProgress () = Some "test"
            return (st1, InProgress<string>.New DateTime.UtcNow timeout checkProgress) |> SecondStepStarted
        | SecondStepStarted (st1, proc) -> 
            match (proc.TimedOut DateTime.UtcNow, proc.CheckResult ()) with
            | (true,_) -> return Expired "step2"
            | (_, Some result) -> return SecondStepComplete (st1, result)
            | (_, None) -> return currentState // keep waiting
        | _ -> return currentState
    }

    let step3 = state {
        // this is a dummy to simulate the processing of third and final step
        let genGuid _ _ = System.Guid.NewGuid ()
        let! currentState = State.get
        match currentState with
        | SecondStepComplete (st1,st2) -> 
            return genGuid st1 st2 |> Ok |> ThirdStepComplete
        | Expired stepName ->
            return sprintf "Timeout Expired on %s" stepName |> Error |> ThirdStepComplete
        | _ -> return currentState
    }

    let doProcess =
        // Reads like imperative code, but it's actually chaining a series of transformations
        // on an invisible state value behind the scenes. No mutable state here.
        state {
            let tO = TimeSpan(30000L) // universal timeout
            let! currentState = State.get
            match currentState with
            | Initial -> return! step1 tO
            | FirstStepComplete _ -> return! step2 tO
            | ThirdStepComplete _ -> return! step3
            | Expired _ -> return! step3
            | _ -> return currentState
        }
    doProcess |> State.run Initial |> fst
    |> printfn "Processing Result: %O"

// //[<StructuralEquality; StructuralComparison>]
// //[<CompiledName("FSharpResult`2")>]
// //[<Struct>]
// type AJourney'TPayload,'AggId> =
//   | Payload of PayloadValue:'TPayload
//   | AggregateId of AggregateIdValue:'AggId

// //[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
// module AJourney =

//   [<CompiledName("Map")>]
//   let map mapping ajourney = match ajourney with AggregateId i -> AggregateId i | Payload p -> Payload (mapping p)

//   [<CompiledName("MapError")>]
//   let mapError mapping ajourney = match ajourney with AggregateId i -> AggregateId (mapping i) | Payload x -> Payload x

//   [<CompiledName("Bind")>]
//   let bind binder ajourney = match ajourney with AggregateId i -> AggregateId i | Payload x -> binder x


// module JourneyBuilder =

//     open System

//     type JourneyBuilder() =
//         member __.Return(x) = Payload x

//         member __.ReturnFrom(m: Result<_, _>) = m

//         member __.Bind(m, f) = Result.bind f m
//         member __.Bind((m, error): (Option<'T> * 'E), f) = m |> ofOption error |> Result.bind f

//         member __.Zero() = None

//         member __.Combine(m, f) = Result.bind f m

//         member __.Delay(f: unit -> _) = f

//         member __.Run(f) = f()

//         member __.TryWith(m, h) =
//             try __.ReturnFrom(m)
//             with e -> h e

//         member __.TryFinally(m, compensation) =
//             try __.ReturnFrom(m)
//             finally compensation()

//         member __.Using(res:#IDisposable, body) =
//             __.TryFinally(body res, fun () -> match res with null -> () | disp -> disp.Dispose())

//         member __.While(guard, f) =
//             if not (guard()) then Ok () else
//             do f() |> ignore
//             __.While(guard, f)

//         member __.For(sequence:seq<_>, body) =
//             __.Using(sequence.GetEnumerator(), fun enum -> __.While(enum.MoveNext, __.Delay(fun () -> body enum.Current)))

//     let journey = new JourneyBuilder()

//     type MyErr = Err1 | Err2

//     let aa : Result<string, MyErr> = 
//         result {
//           let! (a: string) = Ok "a string"
//           printfn "A: %A" a
//         //   let! b = Error Err2
//         //   printfn "B: %A" b
//           let! c = (Some "c string", Err1)
//         //   let! c = (None, Err1)
//           printfn "C: %A" c
//           let d = if true then a else c
//           printfn "D: %A" d
//           return d
//         }