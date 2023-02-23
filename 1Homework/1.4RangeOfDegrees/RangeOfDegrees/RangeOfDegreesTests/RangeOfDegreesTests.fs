module RangeOfDegreesTests

open RangeOfDegrees
open RangeOfDegrees.RangeOfDegrees
open NUnit.Framework

[<Test>]
let Test1 () =
    Assert.AreEqual([ 1.; 2.; 4. ], rangeOfDegreesOf2(0, 2))
    Assert.AreEqual([ 1024.; 2048.; 4096. ], rangeOfDegreesOf2(10, 2))
    Assert.AreEqual([ 64. ], rangeOfDegreesOf2(6, 0))
    Assert.AreEqual(List<double>.Empty, rangeOfDegreesOf2(100, -1))