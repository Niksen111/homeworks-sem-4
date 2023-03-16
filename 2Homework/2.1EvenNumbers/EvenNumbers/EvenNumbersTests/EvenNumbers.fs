module EvenNumbersTests

open EvenNumbers
open NUnit.Framework


[<Test>]
let EvenNumbersFoldWorks () =
    Assert.AreEqual(6, EvenNumbers.EvenNumbersFold([ 1; 2; 3; 4; 5 ]))
    Assert.AreEqual(0, EvenNumbers.EvenNumbersFold([]))
    
[<Test>]
let EvenNumbersFilterWorks () =
    Assert.AreEqual(6, EvenNumbers.EvenNumbersFilter([ 1; 2; 3; 4; 5 ]))
    Assert.AreEqual(0, EvenNumbers.EvenNumbersFilter([]))
    
[<Test>]
let EvenNumbersMapWorks () =
    Assert.AreEqual(6, EvenNumbers.EvenNumbersMap([ 1; 2; 3; 4; 5 ]))
    Assert.AreEqual(0, EvenNumbers.EvenNumbersMap([]))
    