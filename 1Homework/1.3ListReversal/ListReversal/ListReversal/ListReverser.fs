namespace ListReversal

module Reverser =
    let reverse (ls : List<'a>) =
        let rec trueReverse (oldList : List<'a>, newList : List<'a>) =
            match oldList with
            | [] -> newList
            | _ -> trueReverse (oldList.Tail, oldList.Head :: newList)
        
        trueReverse (ls, List<'a>.Empty)