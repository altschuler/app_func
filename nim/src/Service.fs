namespace Nim

module Service =

  open System
  open System.Net
  open Microsoft.FSharp.Control.WebExtensions
  open Nim.Exceptions

  type GameLoader() =
    member this.Fetch(url:string) = async {
      let uri = new System.Uri(url)
      let webClient = new WebClient()
      let! html = webClient.AsyncDownloadString(uri)
      return html
      }

    //member this.Cancel() = this.EventQueue.Post Cancel

  // let fetch (url : string) cb =
  //   [url]
  //   |> Seq.map (fetchAsync cb)
  //   |> Async.Parallel
  //   |> Async.RunSynchronously
  //   |> ignore