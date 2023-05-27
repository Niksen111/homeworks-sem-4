module PhoneDictionary.Utilities

open System.Collections.Generic
open System.IO

type PhoneBook() =
    let mutable dictionaryPN = Dictionary<string, string>()
    let mutable dictionaryNP = Dictionary<string, string>()
    member this.isPhoneNumber (phone: string) =
        let regex =
            System.Text.RegularExpressions.Regex("^\\+[0-9]{6,12}$")
        regex.IsMatch(phone)
        
    member this.printPhoneBookToFile (fileName:string) =
        use writer = new StreamWriter(fileName)
        for kvp in dictionaryPN do
            writer.WriteLine(sprintf "%s %s" kvp.Key kvp.Value)
        writer.Close()

    member this.isPhoneBook (fileName:string) =
        if File.Exists fileName then
            let lines = File.ReadAllLines(fileName)
            let rec isPhoneBookRec i =
                if i = lines.Length then
                    true
                else
                    let split = lines[i].Split ' '
                    if split.Length <> 2 || not (this.isPhoneNumber split[0]) then
                        false
                    else
                        isPhoneBookRec (i + 1)
            isPhoneBookRec 0
        else
            false

    member this.remove phone =
        if not (this.isPhoneNumber phone) || not (dictionaryPN.ContainsKey phone) then
            false
        else
            dictionaryNP.Remove(dictionaryPN.Item(phone)) |> ignore
            dictionaryPN.Remove(phone) |> ignore
            true
    member this.add phone name =
        if this.isPhoneNumber phone then
            if dictionaryPN.ContainsKey(phone) || dictionaryNP.ContainsKey(name) then
                false
            else
                dictionaryPN.Add (phone, name)
                dictionaryNP.Add (name, phone)
                true
        else
            false
    member this.findName phone = if dictionaryPN.ContainsKey phone then dictionaryPN.Item(phone) else "No entry found."
    member this.findPhone name = if dictionaryNP.ContainsKey name then dictionaryNP.Item(name) else "No entry found."
    member this.print =
        if dictionaryPN.Count = 0 then
            printfn "Phone book is empty."
        else 
            for entry in dictionaryPN do
                printfn "%s %s" entry.Key entry.Value
    member this.save (path: string) =
        use writer = new StreamWriter(path)
        for entry in dictionaryPN do
            writer.WriteLine(sprintf "%s %s" entry.Key entry.Value)
        writer.Close()
        true
    member this.load path =
        if this.isPhoneBook path then
            let lines = File.ReadAllLines(path)
            let rec readRec i =
                if i = lines.Length then
                    ()
                else
                    let split = lines[i].Split ' '
                    dictionaryPN.Add(split[0], split[1])
                    dictionaryNP.Add(split[1], split[0])
                    readRec (i + 1)
            readRec 0
            true
        else
            false
            
    member this.clear =
        dictionaryPN.Clear()
        dictionaryNP.Clear()
        
    member this.count =
        dictionaryPN.Count
    
    member this.isEmpty =
        this.count = 0