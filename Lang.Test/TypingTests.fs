module TypingTests

open Xunit
open FParsec
open Microsoft.FSharp.Core.Result

open Typing

[<Fact>]
let ``subst compose`` () =
    Assert.Equal(Subst.size Subst.empty, 0)

    let s11 = Map [ "a", typeBool ] |> Subst
    let s12 = Map [ "b", typeBool ] |> Subst
    Assert.Equal(Subst.compose s11 s12 |> Subst.size, 2)

    let s21 = Map [ "a", typeBool ] |> Subst
    let s22 = Map [ "a", typeInt ] |> Subst
    let s23 = Subst.compose s21 s22
    Assert.Equal(Subst.size s23, 1)
    Assert.Equal(Subst.get "a" s23, Some typeInt)

    let s31 = Map [ "a", typeBool ] |> Subst
    let s32 = Map [ "b", TVar "a" ] |> Subst
    let s33 = Subst.compose s31 s32
    Assert.Equal(Subst.size s33, 2)
    Assert.Equal(Subst.get "a" s33, Some typeBool)
    Assert.Equal(Subst.get "b" s33, Some typeBool)

    let s41 = Map [ "b", TVar "a" ] |> Subst
    let s42 = Map [ "a", typeBool ] |> Subst
    let s43 = Subst.compose s41 s42
    Assert.Equal(Subst.size s43, 2)
    Assert.Equal(Subst.get "a" s43, Some typeBool)
    Assert.Equal(Subst.get "b" s43, Some (TVar "a"))
