module PhoneDictionary.Program

open System
open System.Collections.Generic
open System.IO

module PhoneDictionary =
    
    let rec isPhoneNumber (phone: string) =
        let regex =
            System.Text.RegularExpressions.Regex("^\\+?[0-9]{1,3}\\s?[0-9]{2,3}\\s?[0-9]{2,3}\\s?[0-9]{2,3}$")
        regex.IsMatch(phone)
        
    let printDictionaryToFile (fileName:string) (dict:Dictionary<string, string>) =
        use writer = new StreamWriter(fileName)
        for kvp in dict do
            writer.WriteLine(sprintf "%s %s" kvp.Key kvp.Value)
        writer.Close()

    let isPhoneBook (fileName:string) =
        let lines = File.ReadAllLines(fileName)
        let rec isPhoneBookRec i =
            if i = lines.Length then
                true
            else
                let split = lines[i].Split ' '
                if split.Length <> 2 || not (isPhoneNumber split[0]) then
                    false
                else
                    isPhoneBookRec (i + 1)
        isPhoneBookRec 0

    let dictionaryPN = Dictionary<string, string>()
    let dictionaryNP = Dictionary<string, string>()
    
    let add phone name =
        dictionaryPN.Add (phone, name)
        dictionaryNP.Add (name, phone)
        "Success."
    let remove phone =
        if not (isPhoneNumber phone) || not (dictionaryPN.ContainsKey phone) then
            "No entry found."
        else
            dictionaryNP.Remove(dictionaryPN.Item(phone)) |> ignore
            dictionaryPN.Remove(phone) |> ignore
            "Success."
    let findName phone = if dictionaryPN.ContainsKey phone then dictionaryPN.Item(phone) else "No entry found."
    let findPhone name = if dictionaryNP.ContainsKey name then dictionaryNP.Item(name) else "No entry found."
    let print () =
        for entry in dictionaryPN do
            printfn "%s %s" entry.Key entry.Value
    let save () = ()
    let read path = ()
    let clear () =
        dictionaryPN.Clear()
        dictionaryNP.Clear()
    
    let printHelp () =
        printfn "Commands list:"
        printfn "quit                -quit the app"
        printfn "add <phone> <name>  -add phone number to the dictionary"
        printfn "remove <phone>      -removes the phone from the dictionary"
        printfn "find -n <phone>     -find name of the contact by the phone number"
        printfn "find -p <name>      -find phone of the contact by the name"
        printfn "print               -print all the contacts in the base"
        printfn "save                -save the current data to a file"
        printfn "read <path>         -read the data form the file"
        printfn "clear               -erase all existing phone records"
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
                printfn "%s" (add chunks[1] chunks[2])
            else
                printErr ()
        | "remove" ->
            if chunks.Length = 2 then
                printfn "%s" (remove chunks[1])
            else
                printErr ()
        | "find" ->
            if chunks.Length = 3 then
                match chunks[1] with
                | "-n" ->
                    if isPhoneNumber chunks[2] then
                        printfn "%s" <| findName chunks[2]
                    else
                        printErr ()
                | "-p" -> printfn "%s" <| findPhone chunks[2]
                | _ -> printErr ()
        | "print" -> if chunks.Length = 1 then print () else printErr ()
        | "save" -> if chunks.Length = 1 then save () else printErr ()
        | "read" ->
            if chunks.Length = 2 then
                read chunks[1]
        | "clear" -> if chunks.Length = 1 then clear () else printErr ()
        | "help" -> if chunks.Length = 1 then printHelp () else printErr ()
        | _ -> printErr ()

        commandHandler ()


    printfn "Type 'help' for a list of commands."
    commandHandler ()