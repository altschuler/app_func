namespace Nim

module Main =

  open System

  open Nim.Driver
  open Nim.UI
  open Nim.GUI
  open Nim.CLI

  let parseUI args =
    if Array.exists ((=) "-nw") args
    then
      CLI (
        (loadFn),
        (cancelFn),
        (moveFn),
        (compFn)
        ) :> UI
    else
      GUI (
        (loadFn),
        (cancelFn),
        (moveFn),
        (compFn)
        ) :> UI

  [<EntryPoint>]
  let main args =
    go (parseUI args)

    0
