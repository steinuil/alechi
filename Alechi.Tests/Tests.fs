namespace Alechi.Tests

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting

open Alechi.Compiler

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member __.TestMethodPassing () =
        let out =
            FParsec.CharParsers.run Parse.proc
                "proc ident() { 0 }"
        let out =
            match out with
            | FParsec.CharParsers.Success (x, _, _) -> x
            | _ -> failwith "aaa"
        Assert.AreEqual(
            out,
            Ast.TopLevel.Proc(
                "ident", [],
                Ast.Expression.Constant (Ast.Constant.Int 0L)
            )
        )
