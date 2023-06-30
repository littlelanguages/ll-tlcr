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
let ``infer RecordEmpty`` () =
    assertInferenceEquals TypeEnv.empty "{}" "{}" []

[<Fact>]
let ``infer RecordExtend`` () =
    let env =
        TypeEnv.empty
        |> TypeEnv.extend "x" (Scheme([], TRowExtend("a", typeInt, TRowExtend("b", typeBool, TRowEmpty))))

    assertInferenceEquals env "{}" "{}" []
    assertInferenceEquals env "{a = 10}" "{ a: Int }" []
    assertInferenceEquals env "{a = 10 | {}}" "{ a: Int }" []
    assertInferenceEquals env "{a = 10 | x}" "{ a: Int, a: Int, b: Bool }" []
    assertInferenceEquals env "{c = 10 | x}" "{ c: Int, a: Int, b: Bool }" []
    assertInferenceEquals env "{c = 10; d = 11 | x}" "{ c: Int, d: Int, a: Int, b: Bool }" []
    assertInferenceEquals env "{c = 10 | {d = 11 | x}}" "{ c: Int, d: Int, a: Int, b: Bool }" []

[<Fact>]
let ``infer RecordSelect`` () =
    let env =
        TypeEnv.empty
        |> TypeEnv.extend "x" (Scheme([], TRowExtend("a", typeInt, TRowExtend("b", typeBool, TRowEmpty))))

    assertInferenceEquals env "x" "{ a: Int, b: Bool }" []
    assertInferenceEquals env "x.a" "V0" [ "{ a: V0 | V1 } ~ { a: Int, b: Bool }" ]

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
    | Result.Ok(t, p, cs) ->
        match solve cs p with
        | Result.Ok s, _ -> Assert.Equal(te, Type.prettyPrint (Type.apply s t))
        | Result.Error msg, _ -> failwithf "Failed to solve: %s" msg
    | Result.Error msg -> failwithf "Failed to parse: %s" msg

let private success input expected = assertSolve expected input


[<Fact>]
let ``solve record basics`` () =
    "{ }" |> success "{}"
    "{a = 10; b = True}" |> success "{ a: Int, b: Bool }"
    "{a = 10; b = True}.a" |> success "Int"
    "{a = 10; b = True}.b" |> success "Bool"
    "{a = 10 | { a = False; b = True}}" |> success "{ a: Int, a: Bool, b: Bool }"
    "{a = 10 | { a = False; b = True}}.a" |> success "Int"
    "{a = 10 | { a = False; b = True}}.b" |> success "Bool"

[<Fact>]
let ``solve \\x -> \\y -> \\z -> x + y + z`` () =
    "\\x -> \\y -> \\z -> x + y + z" |> success "Int -> Int -> Int -> Int"

[<Fact>]
let ``solve \\f -> \\g -> \\x -> f (g x)`` () =
    "\\f -> \\g -> \\x -> f (g x)" |> success "(V3 -> V4) -> (V2 -> V3) -> V2 -> V4"

[<Fact>]
let ``solve let rec? compose = \\f -> \\g -> \\x -> f (g x) in compose`` () =
    "let compose = \\f -> \\g -> \\x -> f (g x) in compose"
    |> success "(V6 -> V7) -> (V5 -> V6) -> V5 -> V7"

    "let rec compose = \\f -> \\g -> \\x -> f (g x) in compose"
    |> success "(V9 -> V10) -> (V8 -> V9) -> V8 -> V10"

[<Fact>]
let ``let rec? f = (\\x -> x) in let rec? g = (f True) in f 3`` () =
    "let f = (\\x -> x) in let g = (f True) in f 3" |> success "Int"
    "let rec f = (\\x -> x) in let g = (f True) in f 3" |> success "Int"
    "let f = (\\x -> x) in let rec g = (f True) in f 3" |> success "Int"
    "let rec f = (\\x -> x) in let rec g = (f True) in f 3" |> success "Int"

[<Fact>]
let ``let rec? identity = \\n -> n in identity`` () =
    "let identity = \\n -> n in identity" |> success "V1 -> V1"
    "let rec identity = \\n -> n in identity" |> success "V4 -> V4"

[<Fact>]
let ``let rec? add a b = a + b; succ = add 1 in succ 10`` () =
    "let add a b = a + b; succ = add 1 in succ 10" |> success "Int"
    "let rec add a b = a + b; succ = add 1 in succ 10" |> success "Int"

[<Fact>]
let ``let rec? identity a = a; v = identity 10 in v`` () =
    "let identity a = a; v = identity 10 in v" |> success "Int"
    "let rec identity a = a; v = identity 10 in v" |> success "Int"

[<Fact>]
let ``let rec? identity a = a in let rec? v1 = identity 10; v2 = identity True in v?`` () =
    "let identity a = a in let v1 = identity 10; v2 = identity True in v1" |> success "Int"
    "let identity a = a in let v1 = identity 10; v2 = identity True in v2" |> success "Bool"

    "let identity a = a in let rec v1 = identity 10; v2 = identity True in v1" |> success "Int"
    "let identity a = a in let rec v1 = identity 10; v2 = identity True in v2" |> success "Bool"

    "let rec identity a = a in let v1 = identity 10; v2 = identity True in v1" |> success "Int"
    "let rec identity a = a in let v1 = identity 10; v2 = identity True in v2" |> success "Bool"

    "let rec identity a = a in let rec v1 = identity 10; v2 = identity True in v1" |> success "Int"
    "let rec identity a = a in let rec v1 = identity 10; v2 = identity True in v2" |> success "Bool"

[<Fact>]
let ``let rec? value a b = { a = a; b = b } in value`` () =
    "let value a b = { a = a; b = b } in value" |> success "V2 -> V3 -> { a: V2, b: V3 }"
    "let rec value a b = { a = a; b = b } in value" |> success "V5 -> V6 -> { a: V5, b: V6 }"

[<Fact>]
let ``let value a b = { a = a; b = b }; r = value 10 True in r`` () =
    "let value a b = { a = a; b = b }; r = value 10 True in r"
    |> success "{ a: Int, b: Bool }"

    "let value a b = { a = a; b = b }; r = value 10 True in r.a" |> success "Int"
    "let value a b = { a = a; b = b }; r = value 10 True in r.b" |> success "Bool"
    "let value r = r.a + r.b in value {a = 1; b = 2}" |> success "Int"
    "let value r = r.a + r.b in value" |> success "{ a: Int, b: Int | V6 } -> Int"
    "let value r = r.a in value" |> success "{ a: V3 | V4 } -> V3"
    "let value r = r.a r.b in value" |> success "{ a: V7 -> V8, b: V7 | V9 } -> V8"

[<Fact>]
let ``enclosing scope needed for instantiate`` () =
    "let f x = let g y = y x in g in f" |> success "V4 -> (V4 -> V5) -> V5"
