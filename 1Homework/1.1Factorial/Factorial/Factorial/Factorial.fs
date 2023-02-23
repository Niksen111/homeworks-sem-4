module Factorial.Factorial

let rec Factorial x =
    if x < 0 then
        0
    else if x = 0 || x = 1 then
        1
    else
        x * Factorial (x - 1)
    
        