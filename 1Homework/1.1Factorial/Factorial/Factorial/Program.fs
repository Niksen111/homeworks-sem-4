open System
open Factorial.Factorial

printfn "Enter a 'q' to quit the program."
printfn "Enter an integer number: "
let input = Console.ReadLine()

let writeFactorial (line:string) =
    let isSuccess, number = Int32.TryParse line
    let response =
        if isSuccess then
            $"The factorial of {number} if {Factorial(number)}"
        else
            $"{line} is not a number"

    printfn $"{response}"

let rec cycle line =
    if String.Compare(line, "q") = 0 then
        ()
    else
        writeFactorial line
        printfn "Enter an integer number: "
        let newInput = Console.ReadLine()
        cycle newInput
        

cycle input