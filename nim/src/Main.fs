namespace Nim

module Main =

  open Nim.Core
  open Nim.Util

  [<EntryPoint>]
  let main argv =

    let g = createGame ()

    printGame g

    0
