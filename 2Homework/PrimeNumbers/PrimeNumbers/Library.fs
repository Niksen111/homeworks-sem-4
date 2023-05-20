namespace PrimeNumbers

module Numbers =
    let isPrime x =
        let rec isPrimeRec x i =
            if x % i = 0 then
                false
            else if i * i > x then
                true
            else
                isPrimeRec x (i + 1)
        isPrimeRec x 2
        
    let primeSequence = Seq.initInfinite id |> Seq.filter isPrime