namespace RangeOfDegrees

module RangeOfDegrees =
    let rangeOfDegreesOf2 (n : int, m : int) =
        
        let reverse (ls : List<'a>) =
            let rec trueReverse (oldList : List<'a>, newList : List<'a>) =
                match oldList with
                | [] -> newList
                | _ -> trueReverse (oldList.Tail, oldList.Head :: newList)
            trueReverse (ls, List<'a>.Empty)
            
        let rec listOfDegrees (finalDegree, currentDegree, value, list : List<float>)=
            if finalDegree < currentDegree then
                reverse list 
            else
                listOfDegrees (finalDegree, currentDegree + 1, value * 2., value :: list)
            
        if m < 0 then
            List<double>.Empty
        else
            let firstEl : double = 2. ** n
            listOfDegrees (n+m, n, firstEl, List<double>.Empty)