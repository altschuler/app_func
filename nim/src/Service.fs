namespace Nim

module Service =

  open System
  open System.Net
  open Microsoft.FSharp.Control.WebExtensions
  open Nim.Exceptions

  let fetchAsync cb (url:string) =
    async {
      try
        let uri = new System.Uri(url)
        let webClient = new WebClient()
        let! html = webClient.AsyncDownloadString(uri)

        cb html
      with
        | :? System.FormatException -> raiseParseError url
    }

  let fetch (url : string) cb =
    [url]
    |> Seq.map (fetchAsync cb)
    |> Async.Parallel
    |> Async.RunSynchronously
    |> ignore
