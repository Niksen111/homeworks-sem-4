namespace MiniCrawler

open System
open System.Net
open System.Text.RegularExpressions
open System.Net.Http

module MiniCrawler =
    let getWebPagesInfo (url: string) =
        async {
            try
                let findLinks (inputString: string) = 
                    let regex = Regex(@"<a\shref=""(?<link>http://[^""]+)""")
                    let allMatches = regex.Matches(inputString)
                    let links = [ for m in allMatches -> m.Groups["link"].Value ]
                    links
                
                let client = new HttpClient()
                let! mainPage = client.GetStringAsync(url) |> Async.AwaitTask
                let links = findLinks mainPage
                
                let tasks = links |> List.map (fun (link : string) ->
                        async {
                            try
                                let! content = client.GetStringAsync(link) |> Async.AwaitTask
                                return Some(link, content.Length)
                            with _ ->
                                return None
                        })
                
                return Some(tasks |> Async.Parallel)
            with
            | :? WebException | :? AggregateException ->
                return None
        }