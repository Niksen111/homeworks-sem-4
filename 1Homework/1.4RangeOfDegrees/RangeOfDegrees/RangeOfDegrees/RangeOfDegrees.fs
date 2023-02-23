namespace RangeOfDegrees

module RangeOfDegrees =
    let rangeOfDegreesOf2 (n : int, m : int) =
        
        let reverse (ls : List<'a>) =
            let rec trueReverse (oldList : List<'a>, newList : List<'a>) =
                if oldList.IsEmpty then
                    newList
                else
                    let newList1 = oldList.Head :: newList
                    
                    trueReverse (oldList.Tail, newList1)
            
            trueReverse (ls, List<'a>.Empty)
            
        let rec listOfDegrees (finalDegree, currentDegree, value, list : List<float>)=
            if finalDegree = currentDegree then
                reverse list
            else
                let newList = value :: list
                listOfDegrees (finalDegree, currentDegree + 1, value * 2., newList)
            
        
        if n < m then
            List<float>.Empty
        else
            let firstEl = 2. ** n
            listOfDegrees (n+m, n, firstEl, List<float>.Empty)