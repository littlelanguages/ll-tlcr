module Tests

open Xunit
open Microsoft.FSharp.Core.Result

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
    Assert.Equal(
        Parser.Op(Parser.Op(Parser.Var "x", Parser.Plus, Parser.Var "y"), Parser.Minus, Parser.Var "z")
        |> Core.Ok,
        Parser.parse "x + y - z"
    )

[<Fact>]
let ``parse arithmatic expression`` () =
    Assert.Equal(
        Parser.Op(Parser.Var "x", Parser.Plus, Parser.Op(Parser.Var "y", Parser.Times, Parser.Var "z"))
        |> Core.Ok,
        Parser.parse "x + y * z"
    )

[<Fact>]
let ``parse multiplicative expression`` () =
    Assert.Equal(
        Parser.Op(Parser.Op(Parser.Var "x", Parser.Times, Parser.Var "y"), Parser.Divide, Parser.Var "z")
        |> Core.Ok,
        Parser.parse "x * y / z"
    )

[<Fact>]
let ``parse relational expression`` () =
    Assert.Equal(Parser.Op(Parser.Var "x", Parser.Equals, Parser.Var "y") |> Core.Ok, Parser.parse "x == y")

[<Fact>]
let ``parse if`` () =
    Assert.Equal(
        Parser.If(Parser.LBool true, Parser.Lambda("x", Parser.Var "x"), Parser.LInt 100)
        |> Core.Ok,
        Parser.parse "if (True) (\\x -> x) else 100"
    )

[<Fact>]
let ``parse lambda`` () =
    Assert.Equal(Parser.Lambda("x", Parser.Var "x") |> Core.Ok, Parser.parse "\\x -> x")

[<Fact>]
let ``parse let`` () =
    Assert.Equal(
        Parser.Let([ ("identity", Parser.Lambda("n", Parser.Var "n")) ], Parser.Var "identity")
        |> Core.Ok,
        Parser.parse "let identity n = n in identity"
    )

[<Fact>]
let ``parse let rec`` () =
    Assert.Equal(
        Parser.LetRec([ ("identity", Parser.Lambda("n", Parser.Var "n")) ], Parser.Var "identity")
        |> Core.Ok,
        Parser.parse "let rec identity n = n in identity"
    )

[<Fact>]
let ``parse literal bool`` () =
    Assert.Equal(Parser.LBool true |> Core.Ok, Parser.parse "True")
    Assert.Equal(Parser.LBool false |> Core.Ok, Parser.parse "False")

[<Fact>]
let ``parse literal int`` () =
    Assert.Equal(Parser.LInt 123 |> Core.Ok, Parser.parse "123")

[<Fact>]
let ``parse literal record`` () =
    Assert.Equal(
        [ "a", Parser.LInt 123; "b", Parser.LBool true ] |> Parser.LRecord |> Core.Ok,
        Parser.parse "{ a = 123; b = True}"
    )

[<Fact>]
let ``parse lowerIdentifier`` () =
    Assert.Equal("name" |> Core.Ok, Parser.parseProduction Parser.lowerIdentifier "name")
    Assert.Equal("nAm22e" |> Core.Ok, Parser.parseProduction Parser.lowerIdentifier "nAm22e")
    Assert.True(Parser.parseProduction Parser.lowerIdentifier "Name" |> Core.Result.isError)

[<Fact>]
let ``parse parenthises expression`` () =
    Assert.Equal(Parser.Var "name" |> Core.Ok, Parser.parse "(name)")

[<Fact>]
let ``parse record projection`` () =
    Assert.Equal(
        Parser.RecProj(Parser.RecProj(Parser.Var "name", "field1"), "field2") |> Core.Ok,
        Parser.parse "name.field1.field2"
    )
