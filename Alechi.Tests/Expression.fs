module Alechi.Tests.Expression


open NUnit.Framework
open Alechi.Compiler
open Alechi.Compiler.Ast
open FsUnit
open Utils


[<TestFixture>]
module ``expression blocks`` =
    [<Test>]
    let ``simple expression inside block`` () =
        "{ 0 }"
        |> run Parse.expression
        |> succeeds
        |> should equal (Expression.Constant (Constant.Int 0L))

    [<Test>]
    let ``multiple levels of brackets`` () =
        "{ { { 0 } } }"
        |> run Parse.expression
        |> succeeds
        |> should equal (Expression.Constant (Constant.Int 0L))

    [<Test>]
    let ``if condition inside block`` () =
        "if { true } { false } else { true }"
        |> run Parse.expression
        |> succeeds
        |> should equal (
            Expression.If (
                Expression.Constant (Constant.Bool true),
                Expression.Constant (Constant.Bool false),
                Some (Expression.Constant (Constant.Bool true))
            )
        )

    [<Test>]
    let ``if then branch needs brackets`` () =
        "if { true } false else { true }"
        |> run Parse.expression
        |> fails
        |> should equal (FParsec.ErrorMessage.ExpectedString "{")

    [<Test>]
    let ``if else branch needs brackets`` () =
        "if { () } { false } else true"
        |> run Parse.expression
        |> fails
        |> should equal (FParsec.ErrorMessage.ExpectedString "{")

    [<Test>]
    let ``let needs to be surrounded by brackets`` () =
        "let x = (); ()"
        |> run Parse.expression
        |> succeeds
        |> should equal (
            Expression.Let (
                "x",
                Expression.Constant Constant.Unit,
                Expression.Constant Constant.Unit
            )
        )


[<TestFixture>]
module ``if expression`` =
    [<Test>]
    let ``then and else branches`` () =
        "if 0 { 1 } else { 2 }"
        |> run Parse.expression
        |> succeeds
        |> should equal (
            Expression.If (
                Expression.Constant (Constant.Int 0L),
                Expression.Constant (Constant.Int 1L),
                Some (Expression.Constant (Constant.Int 2L))
            )
        )

    [<Test>]
    let ``missing else branch`` () =
        "if 0 { 1 }"
        |> run Parse.expression
        |> succeeds
        |> should equal (
            Expression.If (
                Expression.Constant (Constant.Int 0L),
                Expression.Constant (Constant.Int 1L),
                None
            )
        )


[<TestFixture>]
module ``let expression`` =
    [<Test>]
    let ``simple let expression`` () =
        "let ident = 0; 2"
        |> run Parse.expression
        |> succeeds
        |> should equal (
            Expression.Let (
                "ident",
                Expression.Constant (Constant.Int 0L),
                Expression.Constant (Constant.Int 2L)
            )
        )

    [<Test>]
    let ``nested in the body`` () =
        "let ident = 2; let ident = 1; 0"
        |> run Parse.expression
        |> succeeds
        |> should equal (
            Expression.Let (
                "ident",
                Expression.Constant (Constant.Int 2L),
                Expression.Let (
                    "ident",
                    Expression.Constant (Constant.Int 1L),
                    Expression.Constant (Constant.Int 0L)
                )
            )
        )

    [<Test>]
    let ``nested in the binding`` () =
        "let id_ = { let id = 0; 1 }; ()"
        |> run Parse.expression
        |> succeeds
        |> should equal (
            Expression.Let (
                "id_",
                Expression.Let (
                    "id",
                    Expression.Constant (Constant.Int 0L),
                    Expression.Constant (Constant.Int 1L)
                ),
                Expression.Constant Constant.Unit
            )
        )

    [<Test>]
    [<Ignore "have to implement this">]
    let ``fails when there's a sequence expression without brackets in the binding`` () =
        "let id_ = let id = 0; 1; 2"
        |> run Parse.expression
        |> fails
        |> ignore
