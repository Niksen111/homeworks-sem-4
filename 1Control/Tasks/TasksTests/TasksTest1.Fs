module TasksTests

open NUnit.Framework
open Tasks.Tasks
open FsUnit

let list1 = [1.0; 2.0; 3.0]

[<Test>]
let SupermapWorks () =
    supermap list1 (fun x -> [x + 1.; x + 2.]) |> should equal [2.0; 3.0; 3.0; 4.0; 4.0; 5.0]
    supermap list1 (fun x -> [x]) |> should equal list1
    supermap list1 (fun x -> []) |> should equal []
    supermap [] (fun x -> [x * 2; x * 3]) |> should haveLength 0
    
let RhombusWorks () =
    rhombus 0 |> should haveLength 1
    (rhombus 0).Head |> should haveLength 0
    Assert.AreEqual(rhombus 1, [['*']])
    rhombus 1 |> should equal [['*']]
    Assert.AreEqual(rhombus 2, [[' '; '*'; ' ']; ['*'; '*'; '*']; [' '; '*'; ' ']])
    rhombus 2 |> should equal [[' '; '*'; ' ']; ['*'; '*'; '*']; [' '; '*'; ' ']]
    rhombus 3 |> should equal [[' '; ' '; '*'; ' '; ' ']; [' '; '*'; '*'; '*'; ' ']; ['*'; '*'; '*'; '*'; '*']; [' '; '*'; '*'; '*'; ' ']; [' '; ' '; '*'; ' '; ' ']]