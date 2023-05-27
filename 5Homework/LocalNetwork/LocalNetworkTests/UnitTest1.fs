module LocalNetworkTests

open NUnit.Framework
open LocalNetwork.LocalNetwork
open LocalNetwork.Computer

let infectGuaranteed =
    function
    | Win -> 1.0
    | Gnu -> 1.0
    | Mac -> 1.0

let neverInfect =
    function
    | Win -> 0.0
    | Gnu -> 0.0
    | Mac -> 0.0

let computersSet() =
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
    (computers, connections)

let network infectChance =
    let comps, conns = computersSet()
    Network(comps, conns, infectChance)

[<Test>]
let Test1 () =
    Assert.Pass()