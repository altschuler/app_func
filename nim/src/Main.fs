namespace Nim

module Main =

  open System

  open Nim.Driver
  open Nim.UI
  open Nim.GUI
  open Nim.CLI

  let parseUI args =
    let fns = (loadFn, cancelFn, moveFn, compFn)
    if Array.exists ((=) "-nw") args
    then CLI fns :> UI
    else GUI fns :> UI

  [<EntryPoint>]
  let main args =
    go (parseUI args)

    0
