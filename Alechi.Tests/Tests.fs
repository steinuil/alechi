namespace Alechi.Tests

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting

open Alechi.Compiler

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member __.TestMethodPassing () =
        let x = new StringReader "proc main() { 0 }"
        Assert.AreEqual(
            Compile.parse x,
            [Ast.Proc("main", [], [Ast.Expr(Ast.Number "0")])]
        )
