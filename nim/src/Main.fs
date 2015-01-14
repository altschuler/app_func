namespace Nim

module Main =

  open Nim.Core

  [<EntryPoint>]
  let main argv =

    let g = new Game()

    g.Board().Print()

    0
