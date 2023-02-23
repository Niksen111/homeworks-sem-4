namespace FibonacciCalculation

module FibonacciCalculation =  
    let rec Fibonacci i =
        let rec CalculateFibonacci (i, currentI, first, second) =
            if currentI = i then
                second
            else
                CalculateFibonacci (i, currentI + 1, second, first + second)
        
        if i < 0 then
            -1
        else if i = 0 then
            0
        else if i = 1 || i = 2 then
            1
        else
            CalculateFibonacci (i, 2, 1, 1)