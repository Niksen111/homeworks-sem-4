module PhoneDictionaryTests

open System.IO
open NUnit.Framework
open PhoneDictionary.Utilities
open FsUnit

let mutable phoneBook = PhoneBook()
let data = ["+78005553535", "Misha"
            "+79608005050", "Masha"
            "+79006661234", "Oleg"]

[<SetUp>]
let SetUp() =
    for phone, name in data do
        phoneBook.add phone name |> ignore

[<Test>]
let ``should print empty dictionary to file``() =
    let fileName = "dictionary.txt"
    phoneBook.clear
    phoneBook.printPhoneBookToFile fileName
    let expectedOutput = ""
    Assert.That(File.ReadAllText(fileName), Is.EqualTo(expectedOutput))
    
[<Test>]
let ``should return true for a valid phone number`` () = 
    let phone = "+12345678910"
    let result = phoneBook.isPhoneNumber phone
    Assert.True(result)
    
[<Test>]
let ``should return false for a phone number with random text`` () = 
    let phone = "+123abc45678910"
    let result = phoneBook.isPhoneNumber phone
    Assert.False(result)
    
[<Test>]
let ``should return false for an invalid phone number format`` () = 
    let phone = "+123 45678910"
    let result = phoneBook.isPhoneNumber phone
    Assert.False(result)
 
[<Test>]
let ``should return true for a valid phone book`` () = 
    let fileName = "./phonebook.txt"
    let localPhoneBook = [|
        "+12345678910 John_Doe"
        "+12345678911 Jane_Doe"
        "+12345678912 Bob_Smith"
    |]
    File.WriteAllLines(fileName, localPhoneBook)
    let result = phoneBook.isPhoneBook fileName
    Assert.True(result)

[<Test>]
let ``should return true for an empty file`` () = 
    let fileName = "./empty.txt"
    File.WriteAllText(fileName, "")
    let result = phoneBook.isPhoneBook fileName
    Assert.True(result)

[<Test>]
let ``should return false for a file with invalid format`` () = 
    let fileName = "invalid.txt"
    let invalidPhoneBook = [|
        "+12345 John_Doe"
        "Jane_Doe"
        "+12345678912"
    |]
    File.WriteAllLines(fileName, invalidPhoneBook)
    let result = phoneBook.isPhoneBook fileName
    Assert.False(result)

[<Test>]
let ``remove works`` () =
    phoneBook.remove "+78005553535" |> should be True
    phoneBook.remove "+78005553535" |> should be False
    phoneBook.remove "2442" |> should be False

[<Test>]
let ``add works`` () =
    phoneBook.add "+77777777777" "Lol" |> should be True
    phoneBook.add "35" "Kek" |> should be False
    phoneBook.add "+77777777777" "Nikita" |> should be False

[<Test>]
let ``finds works`` () =
    phoneBook.findName "+78005553535" |> should equal "Misha"
    phoneBook.findPhone "Misha" |> should equal "+78005553535"
    phoneBook.remove "+78005553535" |> should be True
    phoneBook.findName "+78005553535" |> should equal "No entry found."
    
[<Test>]
let ``count works`` () =
    phoneBook.count |> should equal 3
    phoneBook.add "+77777777777" "Lol" |> should be True
    phoneBook.count |> should equal 4
    phoneBook.remove "+78005553535" |> should be True
    phoneBook.count |> should equal 3

[<Test>]
let ``clear works`` () =
    phoneBook.clear
    phoneBook.count |> should equal 0

[<Test>]    
let ``save and load works`` () =
    phoneBook.save "./book.txt" |> should be True
    phoneBook.isPhoneBook "./book.txt" |> should be True
    phoneBook.clear
    phoneBook.isEmpty |> should be True
    phoneBook.load "./book.txt" |> should be True
    phoneBook.findPhone "Misha" |> should equal "+78005553535"
    phoneBook.findPhone "Oleg" |> should equal "+79006661234"
    phoneBook.findPhone "Masha" |> should equal "+79608005050"