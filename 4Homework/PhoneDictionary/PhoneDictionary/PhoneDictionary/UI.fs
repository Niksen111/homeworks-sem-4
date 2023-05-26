module PhoneDictionary.UI

open System
open System.Collections.Generic
open PhoneDictionary.Utilities.Utilities

module UI =
    let dictionaryPN = Dictionary<string, string>()
    let dictionaryNP = Dictionary<string, string>()
    
    let printHelp () =
        printfn "Commands list:"
        printfn "quit                -quit the app"
        printfn "add <phone> <name>  -add phone number to the dictionary"
        printfn "remove <phone>      -removes the phone from the dictionary"
        printfn "find -n <phone>     -find name of the contact by the phone number"
        printfn "find -p <name>      -find phone of the contact by the name"
        printfn "print               -print all the contacts in the base"
        printfn "save <path>         -save the current data to a file"
        printfn "load <path>         -load the data form the file"
        printfn "clear               -erase all existing phone records"
        printfn "count               -prints the number of entries in the book."
        printfn "help                -command list"
        printfn "<name> must be entered without spaces (using _)"
        printfn "<phone> must be entered in international format"

    let printErr () = printfn "Wrong command."

    let rec commandHandler () =
        printfn "Enter a command:"
        let input = Console.ReadLine()
        let chunks = input.Split ' '

        match chunks[0] with
        | "quit" -> if chunks.Length = 1 then exit 0 else printErr ()
        | "add" ->
            if chunks.Length = 3 then
                printfn "%s" (if add chunks[1] chunks[2] dictionaryPN dictionaryNP  then "Success." else "Incorrect phone number.")
            else
                printErr ()
        | "remove" ->
            if chunks.Length = 2 then
                printfn "%s" (if remove chunks[1] dictionaryPN dictionaryNP then "Success." else "No entry found.")
            else
                printErr ()
        | "find" ->
            if chunks.Length = 3 then
                match chunks[1] with
                | "-n" ->
                    if isPhoneNumber chunks[2] then
                        printfn "%s" <| findName chunks[2] dictionaryPN
                    else
                        printErr ()
                | "-p" -> printfn "%s" <| findPhone chunks[2] dictionaryNP
                | _ -> printErr ()
        | "print" -> if chunks.Length = 1 then print dictionaryPN else printErr ()
        | "save" -> if chunks.Length = 2 then
                        printfn "%s" (if save chunks[1] dictionaryPN then "Success." else "Fail.")
                    else printErr ()
        | "load" ->
            if chunks.Length = 2 then
                printfn "%s" (if (load chunks[1] dictionaryPN dictionaryNP) then "Success." else "File is not a phone book.")
            else
                printErr ()
        | "clear" -> if chunks.Length = 1 then clear dictionaryPN dictionaryNP else printErr ()
        | "count" -> if chunks.Length = 1 then printfn "%d" (count dictionaryPN) else printErr ()
        | "help" -> if chunks.Length = 1 then printHelp () else printErr ()
        | _ -> printErr ()

        commandHandler ()


    printfn "Type 'help' for a list of commands."
    commandHandler ()