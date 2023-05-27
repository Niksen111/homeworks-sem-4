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
          Computer(Gnu, 3, true)
          Computer(Gnu, 4)
          Computer(Mac, 5)
          Computer(Mac, 6)]
        
    let connections =
        [ [computers[0]; computers[2]]
          [computers[0]]
          [computers[0]; computers[4]; computers[5]]
          []
          [computers[2]; computers[5]]
          [computers[2]; computers[4]] ]
    
    List.zip computers connections |> List.map (fun (comp, conn) -> comp.Neighbors <- conn) |> ignore
    
    computers
    
let mockX (x: float) = Mock<System.Random>().Setup(fun x -> <@ x.NextDouble() @>).Returns(x).Create()

let network infectChance =
    Network(computersNet(), infectChance)

[<Test>]
let ``Network is infected in 2 steps`` () =
    let chance = function
        | Win -> 0.001
        | Gnu -> 0.001
        | Mac -> 0.001
    let myNetwork = network chance
    let mock = mockX 0.0001
    let computers = myNetwork.Computers
    computers |> List.map (fun comp -> comp.Random <- mock) |> ignore 
    myNetwork.WorkWileChangeable()
    myNetwork.Iteration |> should equal 3