namespace Nim

module Service =

  open System.Net
  open Microsoft.FSharp.Control.WebExtensions

  let fetchAsync cb (url:string) =
    async {
      try
        printfn "fetching %s..." url
        let uri = new System.Uri(url)
        let webClient = new WebClient()
        let! html = webClient.AsyncDownloadString(uri)
        printfn "got: %s" html
        cb html
      with
        | ex -> printfn "%s" (ex.Message);
    }

  let fetch (url : string) cb =
    [url]
    |> Seq.map (fetchAsync cb)
    |> Async.Parallel
    |> Async.RunSynchronously
    |> ignore
