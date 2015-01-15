namespace Nim

module Main =

  open Nim.Core
  open Nim.Exceptions

  [<EntryPoint>]
  let main argv =

    try
      let g = newGame ()

      printBoard g.Board

      let g' = g.Move (0, 1)
      let g'' = g'.ComputerMove()

      printBoard g'.Board

      printfn "finished: %b" g'.Finished

      let g'' = g'.Move(3, 3).Move(2, 5)//.Move 3 5

      printBoard g''.Board

      printfn "finished: %b, winner: %A" g''.Finished g''.Turn

      g''.Move(1, 1)

      0

    with
      | InvalidMove(s) -> failwith s
      | GameFinished(s) -> failwith s
