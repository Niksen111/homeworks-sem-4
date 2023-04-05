namespace BracketsSequence

open System.Collections.Generic

module MyModule =
    let openBrackets = ['('; '{'; '['; '<']
    let closeBrackets = [')'; '}'; ']'; '>']
    
    let checkBrackets s =
        let rec checkBracketsRec i (stack: Stack<char>) =
            if String.length s = i then
                stack.Count = 0
            else
                match s[i] with
                | c when List.contains c openBrackets -> false
                | c when List.contains c closeBrackets -> false
                
        checkBracketsRec 0 (Stack<char>())