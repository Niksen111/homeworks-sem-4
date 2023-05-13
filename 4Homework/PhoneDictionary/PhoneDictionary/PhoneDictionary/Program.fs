module PhoneDictionary.Program

open System

module PhoneDictionary =
    let add phone = phone
    let findName phone = phone
    let findPhone name = name
    let print = ()
    let save () = ()
    let read path = path

    let printHelp () =
        printfn "Commands list:"
        printfn "quit               -quit the app"
        printfn "add <phone>        -add phone number to the dictionary"
        printfn "find -n <phone>    -find name of the contact by the phone number"
        printfn "find -p <name>     -find phone of the contact by the name"
        printfn "print -a           -print all the contacts in the base"
        printfn "save               -save the current data to a file"
        printfn "read <path>        -read the data form the file"
        printfn "help               -command list"

    let printErr () =
        printfn "Wrong command."

    let rec commandHandler () =
        printfn "Enter a command:"
        let input = Console.ReadLine()
        let chunks = input.Split()
        
        match chunks.Length with
        | 1 ->
            match input with
            | "quit" -> exit 0
            | "help" -> printHelp ()
            | "save" -> save ()
            | _ -> printErr ()
        | 2 ->
            match chunks[0] with
            | "add" -> ()
            | "find" -> ()
            | "print" -> ()
            | "read" -> ()
            | _ -> printErr ()
        | _ -> printErr ()
        commandHandler ()
        
            
    printfn "Type 'help' for a list of commands."
    commandHandler()