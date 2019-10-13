module Alechi.Tests.Expression


open NUnit.Framework
open Alechi.Compiler
open Alechi.Compiler.Ast
open Utils


module ``expression blocks`` =
    [<Test>]
    let ``simple expression inside block`` () =
        Assert.AreEqual(
            Expression.Constant (Constant.Int 0L),
            "{ 0 }"
            |> run Parse.expression |> toSucc
        )

    [<Test>]
    let ``multiple levels of brackets`` () =
        Assert.AreEqual(
            Expression.Constant (Constant.Int 0L),
            "{ { { 0 } } }"
            |> run Parse.expression |> toSucc
        )

    [<Test>]
    let ``if cond inside block`` () =
        Assert.AreEqual (
            Expression.If(
                Expression.Constant (Constant.Bool true),
                Expression.Constant (Constant.Bool false),
                Some (Expression.Constant (Constant.Bool true))
            ),
            "if { true } { false } else { true }"
            |> run Parse.expression |> toSucc
        )

    [<Test>]
    let ``if thenExpr needs brackets`` () =
        Assert.AreEqual(true,
            "if { true } false else { true }"
            |> run Parse.expression |> isErrMsg
        )
