module Tests

open Xunit
open Microsoft.FSharp.Core.Result

let success expected input =
    let input' = Parser.parse input
    let expected' = expected |> Core.Ok

    Assert.Equal(expected', input')

[<Fact>]
let ``parse additive operator`` () =
    Parser.parseProduction Parser.additiveOps "-" |> isOk |> Assert.True
    Parser.parseProduction Parser.additiveOps "+" |> isOk |> Assert.True

[<Fact>]
let ``parse multiplicative operator`` () =
    Parser.parseProduction Parser.multiplicativeOps "*" |> isOk |> Assert.True
    Parser.parseProduction Parser.multiplicativeOps "/" |> isOk |> Assert.True

[<Fact>]
let ``parse additive expression`` () =
    "x + y - z"
    |> success (Parser.Op(Parser.Op(Parser.Var "x", Parser.Plus, Parser.Var "y"), Parser.Minus, Parser.Var "z"))

[<Fact>]
let ``parse arithmatic expression`` () =
    "x + y * z"
    |> success (Parser.Op(Parser.Var "x", Parser.Plus, Parser.Op(Parser.Var "y", Parser.Times, Parser.Var "z")))

[<Fact>]
let ``parse multiplicative expression`` () =
    "x * y / z"
    |> success (Parser.Op(Parser.Op(Parser.Var "x", Parser.Times, Parser.Var "y"), Parser.Divide, Parser.Var "z"))

[<Fact>]
let ``parse relational expression`` () =
    "x == y" |> success (Parser.Op(Parser.Var "x", Parser.Equals, Parser.Var "y"))

[<Fact>]
let ``parse if`` () =
    "if (True) (\\x -> x) else 100"
    |> success (Parser.If(Parser.LBool true, Parser.Lambda("x", Parser.Var "x"), Parser.LInt 100))

[<Fact>]
let ``parse lambda`` () =
    "\\x -> x" |> success (Parser.Lambda("x", Parser.Var "x"))

[<Fact>]
let ``parse let`` () =
    "let identity n = n in identity"
    |> success (Parser.Let([ ("identity", Parser.Lambda("n", Parser.Var "n")) ], Parser.Var "identity"))

[<Fact>]
let ``parse let rec`` () =
    "let rec identity n = n in identity"
    |> success (Parser.LetRec([ ("identity", Parser.Lambda("n", Parser.Var "n")) ], Parser.Var "identity"))

[<Fact>]
let ``parse literal bool`` () =
    "True" |> success (Parser.LBool true)
    "False" |> success (Parser.LBool false)

[<Fact>]
let ``parse literal int`` () = "123" |> success (Parser.LInt 123)

[<Fact>]
let ``parse literal record`` () =
    "{}" |> success Parser.RecordEmpty

    "{ a = 123 }"
    |> success (Parser.RecordExtend("a", Parser.LInt 123, Parser.RecordEmpty))

    "{ a = 123 | e}"
    |> success (Parser.RecordExtend("a", Parser.LInt 123, Parser.Var "e"))

    "{ a = 123; b = True}"
    |> success (
        Parser.RecordExtend("a", Parser.LInt 123, Parser.RecordExtend("b", Parser.LBool true, Parser.RecordEmpty))
    )

    "{ a = 123; b = True | e}"
    |> success (Parser.RecordExtend("a", Parser.LInt 123, Parser.RecordExtend("b", Parser.LBool true, Parser.Var "e")))


[<Fact>]
let ``parse lowerIdentifier`` () =
    Assert.Equal("name" |> Core.Ok, Parser.parseProduction Parser.lowerIdentifier "name")
    Assert.Equal("nAm22e" |> Core.Ok, Parser.parseProduction Parser.lowerIdentifier "nAm22e")
    Assert.True(Parser.parseProduction Parser.lowerIdentifier "Name" |> Core.Result.isError)

[<Fact>]
let ``parse parenthises expression`` () = "(name)" |> success (Parser.Var "name")

[<Fact>]
let ``parse record projection`` () =
    "x.a" |> success (Parser.RecordSelect(Parser.Var "x", "a"))

    "x.a.b"
    |> success (Parser.RecordSelect(Parser.RecordSelect(Parser.Var "x", "a"), "b"))
