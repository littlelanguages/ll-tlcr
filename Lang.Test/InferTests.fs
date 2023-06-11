module InferTests

open Xunit
open Microsoft.FSharp.Core.Result

open Infer
open Typing

let debugAssertInferenceEquals debug env text te cse =
    let p = Pump.init

    let actual = Parser.parse (text) |> map (infer (env, p))

    if debug then
        printfn "Raw: %A" actual

    match actual with
    | Result.Ok(t, _, cs) ->
        if debug then
            printfn "Type: %s" (Type.prettyPrint t)

        Assert.Equal(te, Type.prettyPrint t)

        let csItems =
            let f =
                fun (t1, t2) -> (sprintf "%s ~ %s" (Type.prettyPrint t1) (Type.prettyPrint t2)) in

            List.map f cs

        if debug then
            printfn "Constraints: %A" csItems

        Assert.Equal<string list>(cse, csItems)
    | _ -> Assert.True(isOk actual)

let assertInferenceEquals = debugAssertInferenceEquals false

[<Fact>]
let ``infer Apply`` () =
    let env =
        TypeEnv.empty
        |> TypeEnv.extend "x" (Scheme([ "T" ], TArr(TVar "T", TVar "T")))
        |> TypeEnv.extend "y" (Scheme([], typeInt))

    assertInferenceEquals env "x y" "V1" [ "V0 -> V0 ~ Int -> V1" ]

[<Fact>]
let ``infer If`` () =
    let env =
        TypeEnv.empty
        |> TypeEnv.extend "a" (Scheme([ "S" ], TVar "S"))
        |> TypeEnv.extend "b" (Scheme([], typeInt))
        |> TypeEnv.extend "c" (Scheme([ "T" ], TVar "T"))

    assertInferenceEquals env "if (a) b else c" "Int" [ "V0 ~ Bool"; "Int ~ V1" ]

[<Fact>]
let ``infer Lambda`` () =
    assertInferenceEquals TypeEnv.empty "\\x -> x 10" "V0 -> V1" [ "V0 ~ Int -> V1" ]

[<Fact>]
let ``infer Let`` () =
    assertInferenceEquals TypeEnv.empty "let x = 10; y = x + 1 in y" "Int" [ "Int ~ Int"; "Int ~ Int" ]

    assertInferenceEquals
        TypeEnv.empty
        "let compose = \\f -> \\g -> \\x -> f (g x) in compose"
        "(V6 -> V7) -> (V5 -> V6) -> V5 -> V7"
        [ "V0 ~ V3 -> V4"; "V1 ~ V2 -> V3" ]

[<Fact>]
let ``infer LBool`` () =
    assertInferenceEquals TypeEnv.empty "True" "Bool" []
    assertInferenceEquals TypeEnv.empty "False" "Bool" []

[<Fact>]
let ``infer LInt`` () =
    assertInferenceEquals TypeEnv.empty "123" "Int" []

[<Fact>]
let ``infer Op`` () =
    let env =
        TypeEnv.empty
        |> TypeEnv.extend "a" (Scheme([ "S" ], TVar "S"))
        |> TypeEnv.extend "b" (Scheme([ "S" ], TVar "S"))

    assertInferenceEquals env "a + b" "Int" [ "V0 ~ Int"; "V1 ~ Int" ]
    assertInferenceEquals env "a - b" "Int" [ "V0 ~ Int"; "V1 ~ Int" ]
    assertInferenceEquals env "a * b" "Int" [ "V0 ~ Int"; "V1 ~ Int" ]
    assertInferenceEquals env "a / b" "Int" [ "V0 ~ Int"; "V1 ~ Int" ]
    assertInferenceEquals env "a == b" "Bool" [ "V0 ~ Int"; "V1 ~ Int" ]

let assertSolve text te =
    let p = Pump.init

    let actual = Parser.parse (text) |> map (infer (TypeEnv.empty, p))

    match actual with
    | Result.Ok(t, _, cs) ->
        match solve cs with
        | Result.Ok s -> Assert.Equal(te, Type.prettyPrint (Type.apply s t))
        | Result.Error msg -> failwithf "Failed to solve: %s" msg
    | Result.Error msg -> failwithf "Failed to parse: %s" msg

[<Fact>]
let ``solve \\x -> \\y -> \\z -> x + y + z`` () =
    assertSolve "\\x -> \\y -> \\z -> x + y + z" "Int -> Int -> Int -> Int"

[<Fact>]
let ``solve \\f -> \\g -> \\x -> f (g x)`` () =
    assertSolve "\\f -> \\g -> \\x -> f (g x)" "(V3 -> V4) -> (V2 -> V3) -> V2 -> V4"

[<Fact>]
let ``solve let rec? compose = \\f -> \\g -> \\x -> f (g x) in compose`` () =
    assertSolve "let compose = \\f -> \\g -> \\x -> f (g x) in compose" "(V6 -> V7) -> (V5 -> V6) -> V5 -> V7"
    assertSolve "let rec compose = \\f -> \\g -> \\x -> f (g x) in compose" "(V9 -> V10) -> (V8 -> V9) -> V8 -> V10"

[<Fact>]
let ``let rec? f = (\\x -> x) in let rec? g = (f True) in f 3`` () =
    assertSolve "let f = (\\x -> x) in let g = (f True) in f 3" "Int"
    assertSolve "let rec f = (\\x -> x) in let g = (f True) in f 3" "Int"
    assertSolve "let f = (\\x -> x) in let rec g = (f True) in f 3" "Int"
    assertSolve "let rec f = (\\x -> x) in let rec g = (f True) in f 3" "Int"

[<Fact>]
let ``let rec? identity = \\n -> n in identity`` () =
    assertSolve "let identity = \\n -> n in identity" "V1 -> V1"
    assertSolve "let rec identity = \\n -> n in identity" "V4 -> V4"

[<Fact>]
let ``let rec? add a b = a + b; succ = add 1 in succ 10`` () =
    assertSolve "let add a b = a + b; succ = add 1 in succ 10" "Int"
    assertSolve "let rec add a b = a + b; succ = add 1 in succ 10" "Int"

[<Fact>]
let ``let rec? identity a = a; v = identity 10 in v`` () =
    assertSolve "let identity a = a; v = identity 10 in v" "Int"
    assertSolve "let rec identity a = a; v = identity 10 in v" "Int"

[<Fact>]
let ``let rec? identity a = a in let rec? v1 = identity 10; v2 = identity True in v?`` () =
    assertSolve "let identity a = a in let v1 = identity 10; v2 = identity True in v1" "Int"
    assertSolve "let identity a = a in let v1 = identity 10; v2 = identity True in v2" "Bool"

    assertSolve "let identity a = a in let rec v1 = identity 10; v2 = identity True in v1" "Int"
    assertSolve "let identity a = a in let rec v1 = identity 10; v2 = identity True in v2" "Bool"

    assertSolve "let rec identity a = a in let v1 = identity 10; v2 = identity True in v1" "Int"
    assertSolve "let rec identity a = a in let v1 = identity 10; v2 = identity True in v2" "Bool"

    assertSolve "let rec identity a = a in let rec v1 = identity 10; v2 = identity True in v1" "Int"
    assertSolve "let rec identity a = a in let rec v1 = identity 10; v2 = identity True in v2" "Bool"
