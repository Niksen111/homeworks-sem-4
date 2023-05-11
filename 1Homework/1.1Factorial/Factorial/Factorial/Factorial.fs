module Factorial.Factorial

let Factorial x =
    let rec FacRec x1 acc =
        match x1 with
        | c when c < 0 -> 0
        | 0 -> acc
        | _ -> FacRec (x1 - 1) acc * x1
    FacRec x 1
        