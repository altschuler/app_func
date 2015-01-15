namespace Nim

module Service =

  open System
  open System.Net
  open Microsoft.FSharp.Control.WebExtensions
  open Nim.Exceptions

  type GameLoader() =
    member this.Fetch(url:string) =
      async {
        try
          printfn "fetching %s..." url
          let uri = new System.Uri(url)
          let webClient = new WebClient()
          let! html = webClient.AsyncDownloadString(uri)
          printfn "got: %s" html
          //cb html

        with
          | ex -> printfn "%s" (ex.Message);
      }

    //member this.Cancel() = this.EventQueue.Post Cancel

  // let fetch (url : string) cb =
  //   [url]
  //   |> Seq.map (fetchAsync cb)
  //   |> Async.Parallel
  //   |> Async.RunSynchronously
  //   |> ignore
