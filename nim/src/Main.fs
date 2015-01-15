namespace Nim

module Main =

  open Nim.Core

  [<EntryPoint>]
  let main argv =

    let g = newGame ()

    printBoard g.Board

    let g' = g.Move 0 1
    let g'' = g'.Move 3 2

    printBoard g''.Board

    0
