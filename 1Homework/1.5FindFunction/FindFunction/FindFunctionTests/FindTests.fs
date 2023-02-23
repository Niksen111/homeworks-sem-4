module FindFunctionTests

open FindFunction.FindFunction
open NUnit.Framework

[<Test>]
let FindWorks () =
    let intList = [ 0 .. 10 ]
    let stringList = [ "cat"; "dog"; "helicopter" ]
    Assert.AreEqual(2, find(2, intList))
    Assert.AreEqual(10, find(10, intList))
    Assert.AreEqual(-1, find(666, intList))
    Assert.AreEqual(2, find("helicopter", stringList))
    Assert.AreEqual(-1, find("ship", stringList))