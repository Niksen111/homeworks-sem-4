module Factorial.Factorial

let rec Factorial x =
    match x with
    | _ when x < 0 -> 0
    | 0 -> 1
    | _ -> x * Factorial (x - 1)
        