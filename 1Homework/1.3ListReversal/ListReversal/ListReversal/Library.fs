namespace ListReversal

module Say =
    let reverse (ls : List<'a>) =
        let rec trueReverse (oldList : List<'a>, newList : List<'a>) =
            if oldList.IsEmpty then
                newList
            else
                let newList1 = oldList.Head :: newList
                
                trueReverse (oldList.Tail, newList1)
        
        trueReverse (ls, List<'a>.Empty)
