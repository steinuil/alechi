module Alechi.Tests


open NUnit.Framework
open Alechi.Compiler
open Alechi.Compiler.Ast


let run = FParsec.CharParsers.run


let cast: FParsec.CharParsers.ParserResult<_, _> -> obj = function
    | FParsec.CharParsers.Success (out, _, _) -> out :> obj
    | otherwise -> otherwise :> obj


[<Test>]
let ``proc toplevel statement`` () =
    let out = run Parse.proc "proc ident() { 0 }"
    Assert.AreEqual(
        TopLevel.Proc(
            "ident", [],
            Expression.Constant (Constant.Int 0L)
        ),
        cast out
    )

module ``let expression`` =
    [<Test>]
    let ``let expression`` () =
        Assert.AreEqual(
            Expression.Let("ident",
                Expression.Constant (Constant.Int 0L),
                Expression.Constant (Constant.Int 2L)
            ),
            "let ident = 0; 2"
            |> run Parse.expression |> cast
        )
        Assert.AreEqual(
            Expression.Let("ident",
                Expression.Constant (Constant.Int 2L),
                Expression.Constant (Constant.Int 0L)
            ),
            "let ident=2  ; 0"
            |> run Parse.expression |> cast
        )
        Assert.AreEqual(
            Expression.Let("ident",
                Expression.Constant (Constant.Int 2L),
                Expression.Let("ident",
                    Expression.Constant (Constant.Int 1L),
                    Expression.Constant (Constant.Int 0L)
                )
            ),
            "let ident = 2; let ident = 1; 0"
            |> run Parse.expression |> cast
        )
    
    
[<Test>]
let ``if expression`` () =
    Assert.AreEqual(
        Expression.If(
            Expression.Constant (Constant.Int 0L),
            Expression.Constant (Constant.Int 1L),
            Some (Expression.Constant (Constant.Int 2L))
        ),
        "if 0 { 1 } else { 2 }"
        |> run Parse.expression |> cast
    )
