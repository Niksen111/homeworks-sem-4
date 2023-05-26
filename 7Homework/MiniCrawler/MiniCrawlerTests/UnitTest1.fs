module MiniCrawlerTests

open System
open NUnit.Framework
open FsUnit
open MiniCrawler.MiniCrawler
    
[<Test>]
let ``getPagesInfo works2`` () =
    getWebPagesInfo "https://google.com"
               |> Async.RunSynchronously
               |> Option.get
               |> Async.RunSynchronously
               |> Seq.map (fun linkInfo -> linkInfo.Value)
               |> Seq.toList
               |> should equal [ ("http://www.google.ru/intl/ru/services/", 75846)]
    
[<Test>]
let ``bad page`` () =
    getWebPagesInfo "http://bad.url.kek.lol"
               |> Async.RunSynchronously |> should equal None

                