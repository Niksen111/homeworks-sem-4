module LocalNetworkTests

open NUnit.Framework
open LocalNetwork.LocalNetwork
open LocalNetwork.Computer
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
          [] ]
    
    List.zip computers connections |> List.map (fun (comp, conn) -> comp.Neighbors <- conn) |> ignore
    
    computers

let network infectChance random =
    Network(computersNet(), infectChance, random)

[<Test>]
let ``Only neighboring computers are infected`` () =
    let chance = function
        | Win -> 1.0
        | Gnu -> 1.0
        | Mac -> 1.0
    let myNetwork = network chance (System.Random().NextDouble)
    let computers = myNetwork.Computers
    let after1Step = [false; true; false; true; false; true; false; false]
    let after2Step = [true; true; true; true; true; true; true; false]
        
    myNetwork.Step()
    let infected1 = computers |> List.map (fun comp -> comp.IsInfected)
    infected1 |> should equal after1Step 
    myNetwork.Changeable |> should be True
    
    myNetwork.Step()
    let infected2 = computers |> List.map (fun comp -> comp.IsInfected)
    infected2 |> should equal after2Step 
    myNetwork.Changeable |> should be False
        
[<Test>]
let ``Network is not infected if the chance is 0``() =
    let chance = function
        | Win -> 0.0
        | Gnu -> 0.0
        | Mac -> 0.0
    let myNetwork = network chance (System.Random().NextDouble)
    let computers = myNetwork.Computers
    let afterStep = [false; false; false; true; false; false; false; false]

    myNetwork.Changeable |> should be False
    myNetwork.Step()
    let infected = computers |> List.map (fun comp -> comp.IsInfected)
    infected |> should equal afterStep 
    myNetwork.Changeable |> should be False