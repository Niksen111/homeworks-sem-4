module FibonacciTests

open FibonacciCalculation
open NUnit.Framework

[<Test>]
let FibonacciWorks () =
    Assert.AreEqual(-1, FibonacciCalculation.Fibonacci(-666))
    Assert.AreEqual(0, FibonacciCalculation.Fibonacci(0))
    Assert.AreEqual(1, FibonacciCalculation.Fibonacci(1))
    Assert.AreEqual(1, FibonacciCalculation.Fibonacci(2))
    Assert.AreEqual(2, FibonacciCalculation.Fibonacci(3))
    Assert.AreEqual(1597, FibonacciCalculation.Fibonacci(17))
    Assert.AreEqual(102334155, FibonacciCalculation.Fibonacci(40))