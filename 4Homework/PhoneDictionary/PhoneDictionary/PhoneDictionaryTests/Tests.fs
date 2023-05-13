module PhoneDictionaryTests

open System.IO
open NUnit.Framework
open PhoneDictionary.Program.PhoneDictionary
open System.Collections.Generic
open FsUnit

[<Test>]
let ``should print dictionary to file``() =
    // arrange
    let fileName = "dictionary.txt"
    let dict = Dictionary<string, string>()
    dict.Add("hello", "world")
    dict.Add("foo", "bar")

    // act
    printDictionaryToFile fileName dict

    // assert
    let expectedOutput = ["hello world"; "foo bar\r\n"] |> String.concat "\r\n"
    Assert.That(File.ReadAllText(fileName), Is.EqualTo(expectedOutput))

[<Test>]
let ``should print empty dictionary to file``() =
    // arrange
    let fileName = "dictionary.txt"
    let dict = Dictionary<string, string>()

    // act
    printDictionaryToFile fileName dict

    // assert
    let expectedOutput = ""
    Assert.That(File.ReadAllText(fileName), Is.EqualTo(expectedOutput))
    
[<Test>]
let ``should return true for a valid phone number`` () = 
    // arrange
    let phone = "+12345678910"
    
    // act
    let result = isPhoneNumber phone
    
    // assert
    Assert.True(result)
    
[<Test>]
let ``should return false for a phone number with random text`` () = 
    // arrange
    let phone = "+123abc45678910"
    
    // act
    let result = isPhoneNumber phone
    
    // assert
    Assert.False(result)
    
[<Test>]
let ``should return false for an invalid phone number format`` () = 
    // arrange
    let phone = "+123 45678910"
    
    // act
    let result = isPhoneNumber phone
    
    // assert
    Assert.False(result)
 
[<Test>]
let ``should return true for a valid phone book`` () = 
    // arrange
    let fileName = "./phonebook.txt"
    let phoneBook = [|
        "+12345678910 John_Doe"
        "+12345678911 Jane_Doe"
        "+12345678912 Bob_Smith"
    |]
    File.WriteAllLines(fileName, phoneBook)
    
    // act
    let result = isPhoneBook fileName
    
    // assert
    Assert.True(result)

[<Test>]
let ``should return true for an empty file`` () = 
    // arrange
    let fileName = "./empty.txt"
    File.WriteAllText(fileName, "")
    
    // act
    let result = isPhoneBook fileName
    
    // assert
    Assert.True(result)

[<Test>]
let ``should return false for a file with invalid format`` () = 
    // arrange
    let fileName = "invalid.txt"
    let invalidPhoneBook = [|
        "+12345 John_Doe"
        "Jane_Doe"
        "+12345678912"
    |]
    File.WriteAllLines(fileName, invalidPhoneBook)
    
    // act
    let result = isPhoneBook fileName
    
    // assert
    Assert.False(result)
    