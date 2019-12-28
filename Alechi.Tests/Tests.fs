module Alechi.Tests.Base


open NUnit.Framework
open Alechi.Compiler
open Alechi.Compiler.Ast
open FsUnit
open Utils


[<TestFixture>]
module ``ident`` =
    [<Test>]
    let ``a-z attributes`` () =
        "ident"
        |> run Parse.ident
        |> succeeds
        |> should equal "ident"

    [<Test>]
    let ``with underscores`` () =
        "a_b"
        |> run Parse.ident
        |> succeeds
        |> should equal "a_b"

    [<Test>]
    let ``should be at least one character long`` () =
        ""
        |> run Parse.ident
        |> fails
        |> ignore


[<TestFixture (true, "true")>]
[<TestFixture (false, "false")>]
type ``boolean constants`` (b: bool, str: string) =
    [<Test>]
    member __.``parses to the constant`` () =
        str
        |> run Parse.constant
        |> succeeds
        |> should equal (Constant.Bool b)


[<Test>]
let ``unit literal`` () =
    "()"
    |> run Parse.constant
    |> succeeds
    |> should equal Constant.Unit


[<TestFixture>]
module ``proc statement`` =
    [<Test>]
    let ``no arguments`` () =
        "proc ident() { 0 }"
        |> run Parse.topLevel
        |> succeeds
        |> should equal (
            TopLevel.Proc(
                "ident",
                [],
                Expression.Constant (Constant.Int 0L)
            )
        )



[<Test>]
let ``import statement`` () =
    "import \"string\""
    |> run Parse.topLevel
    |> succeeds
    |> should equal (TopLevel.Import "string")
