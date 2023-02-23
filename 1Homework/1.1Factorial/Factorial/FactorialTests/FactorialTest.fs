module TestProject1

open Factorial.Factorial
open NUnit.Framework

[<Test>]
let FactorialWorks () =
    Assert.AreEqual(0, Factorial(-1))
    Assert.AreEqual(1, Factorial(0))
    Assert.AreEqual(1, Factorial(1))
    Assert.AreEqual(479001600, Factorial(12))