namespace FibonacciCalculation

module FibonacciCalculation =  
    let rec Fibonacci i =
        let rec CalculateFibonacci (i, currentI, first, second) =
            if currentI = i then
                second
            else
                CalculateFibonacci (i, currentI + 1, second, first + second)
        match i with
        | _ when i < 0 -> -1
        | 0 -> 0
        | 1 -> 1
        | 2 -> 1
        | _ -> CalculateFibonacci (i, 2, 1, 1)        