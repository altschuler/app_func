namespace Nim

module Main =

  open System

  open Nim.Driver
  open Nim.UI
  open Nim.GUI
  open Nim.CLI


  [<EntryPoint>]
  let main args =

    if Array.exists ((=) "-nw") args
    then
      go (CLI (
        (loadFn),
        (cancelFn),
        (moveFn),
        (compFn)
        ) :> UI)
    else
      go (GUI (
        (loadFn),
        (cancelFn),
        (moveFn),
        (compFn)
        ) :> UI)

    0
