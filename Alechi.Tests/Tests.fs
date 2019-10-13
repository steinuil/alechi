module Alechi.Tests.T


open NUnit.Framework
open Alechi.Compiler
open Alechi.Compiler.Ast


let run = FParsec.CharParsers.run


let toSucc: FParsec.CharParsers.ParserResult<_, _> -> obj = function
    | FParsec.CharParsers.Success (out, _, _) -> out :> obj
    | otherwise -> otherwise :> obj


let isErrMsg: FParsec.CharParsers.ParserResult<_, _> -> obj = function
    | FParsec.CharParsers.Failure _ -> true :> obj
    | otherwise -> otherwise :> obj


module ``ident tests`` =
    [<Test>]
    let ``aaa`` () =
        Assert.AreEqual(
            "ident",
            "ident" |> run Parse.ident |> toSucc
        )
        Assert.AreEqual(
            "id",
            "id" |> run Parse.ident |> toSucc
        )

    [<Test>]
    let ``underscores are allowed`` () =
        Assert.AreEqual(
            "a_b",
            "a_b" |> run Parse.ident |> toSucc
        )

    [<Test>]
    let ``does not match an empty identifier`` () =
        Assert.AreEqual(
            true,
            "" |> run Parse.ident |> isErrMsg
        )


[<Test>]
let ``proc toplevel statement`` () =
    Assert.AreEqual(
        TopLevel.Proc(
            "ident", [],
            Expression.Constant (Constant.Int 0L)
        ),
        "proc ident() { 0 }"
        |> run Parse.topLevel |> toSucc
    )


module ``constant tests`` =
    [<Test>]
    let ``boolean`` () =
        Assert.AreEqual(
            Constant.Bool true,
            "true" |> run Parse.constant |> toSucc
        )
        Assert.AreEqual(
            Constant.Bool false,
            "false" |> run Parse.constant |> toSucc
        )

    [<Test>]
    let ``unit`` () =
        Assert.AreEqual(
            Constant.Unit,
            "()" |> run Parse.constant |> toSucc
        )

    [<Test>]
    let ``string`` () =
        Assert.AreEqual(
            Constant.String "string",
            "\"string\"" |> run Parse.constant |> toSucc
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
            |> run Parse.expression |> toSucc
        )
        Assert.AreEqual(
            Expression.Let("ident",
                Expression.Constant (Constant.Int 2L),
                Expression.Constant (Constant.Int 0L)
            ),
            "let ident=2  ; 0"
            |> run Parse.expression |> toSucc
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
            |> run Parse.expression |> toSucc
        )

    [<Test>]
    let ``asdas`` () =
        Assert.AreEqual(
            Expression.Let("test",
                Expression.Identifier "one",
                Expression.Identifier "two"
            ),
            "let test = one; two"
            |> run Parse.expression |> toSucc
        )

    [<Test>]
    let ``can't use a keyword there`` () =
        Assert.AreEqual(
            true,
            "let let = 0; 1"
            |> run Parse.expression |> isErrMsg
        )

    [<Test>]
    let ``ident that starts with a keyword`` () =
        Assert.AreEqual(
            Expression.Let("leta",
                Expression.Identifier "one",
                Expression.Identifier "two"
            ),
            "let leta = one; two"
            |> run Parse.expression |> toSucc
        )
