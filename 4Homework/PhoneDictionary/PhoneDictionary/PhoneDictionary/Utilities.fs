module PhoneDictionary.Utilities

open System
open System.Collections.Generic
open System.IO

module Utilities =
    let isPhoneNumber (phone: string) =
        let regex =
            System.Text.RegularExpressions.Regex("^\\+[0-9]{6,12}$")
        regex.IsMatch(phone)
        
    let printDictionaryToFile (fileName:string) (dict:Dictionary<string, string>) =
        use writer = new StreamWriter(fileName)
        for kvp in dict do
            writer.WriteLine(sprintf "%s %s" kvp.Key kvp.Value)
        writer.Close()

    let isPhoneBook (fileName:string) =
        if File.Exists fileName then
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
        else
            false
    let add phone name (dictionaryPN: Dictionary<String, String>) (dictionaryNP: Dictionary<String, String>)=
        if isPhoneNumber phone then
            dictionaryPN.Add (phone, name)
            dictionaryNP.Add (name, phone)
            true
        else
            false
    let remove phone (dictionaryPN: Dictionary<String, String>) (dictionaryNP: Dictionary<String, String>)=
        if not (isPhoneNumber phone) || not (dictionaryPN.ContainsKey phone) then
            false
        else
            dictionaryNP.Remove(dictionaryPN.Item(phone)) |> ignore
            dictionaryPN.Remove(phone) |> ignore
            true
    let findName phone (dictionaryPN: Dictionary<String, String>) = if dictionaryPN.ContainsKey phone then dictionaryPN.Item(phone) else "No entry found."
    let findPhone name (dictionaryNP: Dictionary<String, String>) = if dictionaryNP.ContainsKey name then dictionaryNP.Item(name) else "No entry found."
    
    let print (dictionaryPN: Dictionary<String, String>) =
        if dictionaryPN.Count = 0 then
            printfn "Phone book is empty."
        else 
            for entry in dictionaryPN do
                printfn "%s %s" entry.Key entry.Value
    let save (path: string) (dictionaryPN: Dictionary<String, String>) =
        use writer = new StreamWriter(path)
        for entry in dictionaryPN do
            writer.WriteLine(sprintf "%s %s" entry.Key entry.Value)
        writer.Close()
        true
    let load path (dictionaryPN: Dictionary<String, String>) (dictionaryNP: Dictionary<String, String>)=
        if isPhoneBook path then
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
            
    let clear (dictionaryPN: Dictionary<String, String>) (dictionaryNP: Dictionary<String, String>) =
        dictionaryPN.Clear()
        dictionaryNP.Clear()
        
    let count (dictionaryPN: Dictionary<String, String>) =
        dictionaryPN.Count