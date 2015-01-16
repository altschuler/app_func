namespace Nim

module Main =

  open System

  open Nim.Driver
  open Nim.UI
  open Nim.GUI


  [<EntryPoint>]
  let main args =

    // cli
    if Array.exists ((=) "-nw") args
    then
      printfn "Ja Simon, vi skal ha et CLI"

    // gui
    else
      let ui =
        GUI (
          (fun url  -> ev.Post (Load url)),
          (fun _    -> ev.Post Cancel),
          (fun move -> ev.Post (HumanMove move)),
          (fun _    -> ev.Post ComputerMove)
          ) :> UI

      go ui

    0
