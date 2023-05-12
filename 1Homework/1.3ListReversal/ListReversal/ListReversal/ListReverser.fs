namespace ListReversal

module Reverser =
    let reverse (ls : List<'a>) =
        let rec trueReverse (oldList : List<'a>, newList : List<'a>) =
            match oldList with
            | [] -> newList
            | head::tail -> trueReverse (tail, head :: newList)
        
        trueReverse (ls, List<'a>.Empty)