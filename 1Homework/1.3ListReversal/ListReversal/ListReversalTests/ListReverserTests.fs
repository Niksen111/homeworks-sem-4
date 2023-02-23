module ListReversalTests

open NUnit.Framework
open ListReversal.Reverser

[<Test>]
let ReverseWorks () =
    Assert.AreEqual(List<int>.Empty, reverse(List<int>.Empty))
    Assert.AreEqual([ 5; 4; 3; 2; 1 ], reverse([ 1 .. 5 ]))
    Assert.AreEqual([ "a"; "b"; "c" ], reverse([ "c"; "b"; "a" ]))