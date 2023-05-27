module LocalNetworkTests

open NUnit.Framework
open LocalNetwork.LocalNetwork
open LocalNetwork.Computer
open Foq
open FsUnit

let computersNet() =
    let computers = 
        [ Computer(Win, 1)
          Computer(Win, 2)
          Computer(Win, 3)
          Computer(Gnu, 4, true)
          Computer(Gnu, 5)
          Computer(Mac, 6)
          Computer(Mac, 7)
          Computer(Mac, 8)]
        
    let connections =
        [ [computers[1]]
          [computers[0]; computers[2]; computers[3]]
          [computers[1]]
          [computers[1]; computers[5]]
          [computers[5]]
          [computers[3]; computers[4]; computers[6]]
          [computers[5]]
          []]
    
    List.zip computers connections |> List.map (fun (comp, conn) -> comp.Neighbors <- conn) |> ignore
    
    computers
    
let mockX (x: float) = Mock<System.Random>().Setup(fun x -> <@ x.NextDouble() @>).Returns(x).Create()

let network infectChance =
    Network(computersNet(), infectChance)

[<Test>]
let ``Network algorithm works right`` () =
    let chance = function
        | Win -> 0.001
        | Gnu -> 0.001
        | Mac -> 0.001
    let myNetwork = network chance
    let mock = mockX 0.0001
    let computers = myNetwork.Computers
    computers |> List.map (fun comp -> comp.Random <- mock) |> ignore 
    
    myNetwork.Step()
    computers[1].IsInfected |> should be True
    computers[5].IsInfected |> should be True
    computers[0].IsInfected |> should be False
    computers[2].IsInfected |> should be False
    computers[4].IsInfected |> should be False
    computers[6].IsInfected |> should be False
    
    myNetwork.WorkWileChangeable()
    myNetwork.LastIteration |> should equal 3
    // Infected all but comp 8
    for comp in computers do
        comp.IsInfected |> should equal (comp.Id <> 8)
        
[<Test>]
let ``Network is not infected if the chance is 0``() =
    let chance = function
        | Win -> 0.0
        | Gnu -> 0.0
        | Mac -> 0.0
    let myNetwork = network chance
    myNetwork.WorkWileChangeable()
    myNetwork.LastIteration |> should equal 1
    // Infected no one but comp 4
    for comp in myNetwork.Computers do
        comp.IsInfected |> should equal (comp.Id <> 4 |> not)