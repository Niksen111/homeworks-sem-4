module PrimeNumbersTests

open NUnit.Framework
open PrimeNumbers.Numbers
open FsUnit

[<Test>]
let ``primeSequence works``() =
        let expected = [2; 3; 5; 7; 11; 13; 17; 19; 23; 29]
        let actual = primeSequence |> Seq.take 10 |> Seq.toList
        actual |> should equal expected
        
[<Test>]
let ``first 100000 numbers are prime``() =
    let seq100000 = primeSequence |> Seq.take 100000
    let seqLength = seq100000 |> Seq.length
    seq100000 |> Seq.filter(isPrime) |> Seq.length |> should equal seqLength
        
[<Test>]
let ``Number smaller than 2 should not be considered prime``() =
    isPrime 1 |> should equal false
    isPrime 0 |> should equal false
    isPrime -1 |> should equal false
    isPrime -2 |> should equal false
    isPrime -93 |> should equal false

[<Test>]
let ``Prime is considered prime``() =
    isPrime(2) |> should equal true
    isPrime(7) |> should equal true
    isPrime(11) |> should equal true
    isPrime(23) |> should equal true
    isPrime(27644437) |> should equal true

[<Test>]
let ``Not prime numbers are not prime``() =
    isPrime(4) |> should equal false
    isPrime(6) |> should equal false
    isPrime(9) |> should equal false
    isPrime(99) |> should equal false
    isPrime(1866326401) |> should equal false