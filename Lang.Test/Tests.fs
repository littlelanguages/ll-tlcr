module Tests

open Xunit
open FParsec
open Microsoft.FSharp.Core.Result

let test p str =
    match run p str with
    | Success(result, _, l) -> Core.Ok result
    | Failure(msg, _, _) -> Core.Error msg

[<Fact>]
let ``parse additive operator`` () =
    test Parser.additiveOps "-" |> isOk |> Assert.True
    test Parser.additiveOps "+" |> isOk |> Assert.True

[<Fact>]
let ``parse multiplicative operator`` () =
    test Parser.multiplicativeOps "*" |> isOk |> Assert.True
    test Parser.multiplicativeOps "/" |> isOk |> Assert.True

[<Fact>]
let ``parse additive expression`` () =
    Assert.Equal(
        Parser.Op(Parser.Op(Parser.Var "x", Parser.Plus, Parser.Var "y"), Parser.Minus, Parser.Var "z")
        |> Core.Ok,
        test Parser.expression "x + y - z"
    )

[<Fact>]
let ``parse arithmatic expression`` () =
    Assert.Equal(
        Parser.Op(Parser.Var "x", Parser.Plus, Parser.Op(Parser.Var "y", Parser.Times, Parser.Var "z"))
        |> Core.Ok,
        test Parser.expression "x + y * z"
    )

[<Fact>]
let ``parse multiplicative expression`` () =
    Assert.Equal(
        Parser.Op(Parser.Op(Parser.Var "x", Parser.Times, Parser.Var "y"), Parser.Divide, Parser.Var "z")
        |> Core.Ok,
        test Parser.expression "x * y / z"
    )

[<Fact>]
let ``parse relational expression`` () =
    Assert.Equal(Parser.Op(Parser.Var "x", Parser.Equals, Parser.Var "y") |> Core.Ok, test Parser.expression "x == y")

[<Fact>]
let ``parse if`` () =
    Assert.Equal(
        Parser.If(Parser.LBool true, Parser.Lambda("x", Parser.Var "x"), Parser.LInt 100)
        |> Core.Ok,
        test Parser.expression "if (True) (\\x -> x) else 100"
    )

[<Fact>]
let ``parse lambda`` () =
    Assert.Equal(Parser.Lambda("x", Parser.Var "x") |> Core.Ok, test Parser.expression "\\x -> x")

[<Fact>]
let ``parse let`` () =
    Assert.Equal(
        Parser.Let([ ("identity", Parser.Lambda("n", Parser.Var "n")) ], Parser.Var "identity")
        |> Core.Ok,
        test Parser.expression "let identity n = n in identity"
    )

[<Fact>]
let ``parse let rec`` () =
    Assert.Equal(
        Parser.LetRec([ ("identity", Parser.Lambda("n", Parser.Var "n")) ], Parser.Var "identity")
        |> Core.Ok,
        test Parser.expression "let rec identity n = n in identity"
    )

[<Fact>]
let ``parse literal bool`` () =
    Assert.Equal(Parser.LBool true |> Core.Ok, test Parser.expression "True")
    Assert.Equal(Parser.LBool false |> Core.Ok, test Parser.expression "False")

[<Fact>]
let ``parse literal int`` () =
    Assert.Equal(Parser.LInt 123 |> Core.Ok, test Parser.expression "123")

[<Fact>]
let ``parse lowerIdentifier`` () =
    Assert.Equal("name" |> Core.Ok, test Parser.lowerIdentifier "name")
    Assert.Equal("nAm22e" |> Core.Ok, test Parser.lowerIdentifier "nAm22e")
    Assert.True(test Parser.lowerIdentifier "Name" |> Core.Result.isError)

[<Fact>]
let ``parse parenthises expression`` () =
    Assert.Equal(Parser.Var "name" |> Core.Ok, test Parser.expression "(name)")
