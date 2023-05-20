namespace PrimeNumbers

open System

module Numbers =
    let isPrime (x: int) =
        let rec isPrimeRec num i =
            if i * i > num then
                true
            else if num % i = 0 then
                false
            else
                isPrimeRec num (i + 1)
        if x < 2 then
            false
        else
            isPrimeRec x 2
        
    let primeSequence = Seq.initInfinite id |> Seq.filter isPrime