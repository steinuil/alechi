namespace Alechi.Tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member __.TestMethodPassing () =
        Assert.IsTrue(true);
