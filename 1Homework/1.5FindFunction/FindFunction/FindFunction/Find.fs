namespace FindFunction

module FindFunction =
    let find (element, list : List<'a>) =
        let rec realFind (element, myList : List<'a>, i) =
            if myList.Head = element then
                i
            else if myList.Tail = List<'a>.Empty then
                -1
            else
                realFind (element, myList.Tail, i + 1)
                
        realFind (element, list, 0)