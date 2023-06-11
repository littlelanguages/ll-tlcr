module InterpreterTests

open Xunit

open Typing

let assertExecute text (expected: string) =
    let p = Pump.init

    let ast = Parser.parse (text)

    match ast with
    | Result.Ok e ->
        let t, _, cs = Infer.infer (TypeEnv.empty, p) e

        match Infer.solve cs with
        | Result.Ok s ->
            let t = Type.apply s t
            let actual = Interpreter.evaluate e (Interpreter.emptyEnv ())
            Assert.Equal(expected, Interpreter.Value.prettyPrintWith actual t)
        | Result.Error msg -> Assert.Equal(expected, msg)

    | Result.Error msg -> Assert.Equal(expected, msg)

[<Fact>]
let ``App`` () =
    assertExecute "(\\a -> \\b -> a + b) 10 20" "30: Int"

[<Fact>]
let ``If`` () =
    assertExecute "if (True) 1 else 2" "1: Int"
    assertExecute "if (False) 1 else 2" "2: Int"

[<Fact>]
let ``Lam`` () =
    assertExecute "\\a -> \\b -> a + b" "function: Int -> Int -> Int"

[<Fact>]
let ``Let`` () =
    assertExecute "let add a b = a + b ; incr = add 1 in incr 10" "11: Int"

[<Fact>]
let ``LetRec`` () =
    assertExecute "let rec fact n = if (n == 0) 1 else n * (fact (n - 1)) in fact" "function: Int -> Int"
    assertExecute "let rec fact n = if (n == 0) 1 else n * (fact (n - 1)) in fact 5" "120: Int"

    assertExecute
        "let rec isOdd n = if (n == 0) False else isEven (n - 1); isEven n = if (n == 0) True else isOdd (n - 1) in isEven 5"
        "false: Bool"

    assertExecute
        "let rec isOdd n = if (n == 0) False else isEven (n - 1); isEven n = if (n == 0) True else isOdd (n - 1) in isOdd 5"
        "true: Bool"

[<Fact>]
let ``LBool`` () =
    assertExecute "True" "true: Bool"
    assertExecute "False" "false: Bool"

[<Fact>]
let ``LInt`` () =
    assertExecute "123" "123: Int"
    assertExecute "-123" "-123: Int"

[<Fact>]
let ``Op`` () =
    assertExecute "1 == 2" "false: Bool"
    assertExecute "2 == 2" "true: Bool"

    assertExecute "3 + 2" "5: Int"
    assertExecute "3 - 2" "1: Int"
    assertExecute "3 * 2" "6: Int"
    assertExecute "9 / 2" "4: Int"

[<Fact>]
let ``Var`` () =
    assertExecute "let x = 1 in x" "1: Int"
    assertExecute "let x = True in x" "true: Bool"
    assertExecute "let x = \\a -> a in x" "function: V1 -> V1"
