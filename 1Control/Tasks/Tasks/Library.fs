namespace Tasks

module Tasks =
    let rec supermap l f =
        match l with
        | [] -> []
        | head :: tail -> f head @ supermap tail f

    let rhombus n =
        n