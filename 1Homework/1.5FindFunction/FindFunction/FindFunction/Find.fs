namespace FindFunction

module FindFunction =
    let find (element, list : List<'a>) =
        let rec realFind (element, myList : List<'a>, i) =
            match myList with
            | [] -> -1
            | head :: _ when head = element -> i
            | _ :: tail -> realFind (element, tail, i + 1)
                
        realFind (element, list, 0)