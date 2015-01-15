namespace Nim

module Main =

  open Nim.Core
  open Nim.Utils
  open Nim.Exceptions
  open Nim.Service

  [<EntryPoint>]
  let main argv =

    let play (s:string) =

      let g = new Game(First, new Board(parseInts s))

      printBoard g.Board

      let g' = g.Move (0, 1)
      g'.Warn()
      let g'' = g'.ComputerMove()
      g''.Warn()

      printBoard g'.Board

      printfn "finished: %b" g'.Finished

      let g'' = g'.Move(3, 3).Move(2, 5)//.Move 3 5

      printBoard g''.Board

      printfn "finished: %b, winner: %A" g''.Finished g''.Turn
      ignore <| g''.Move(1, 1)

    try

      fetch "http://www2.compute.dtu.dk/~mire/nim.game" (play)

      0

    with
      | InvalidMove(s) -> failwith s
      | GameFinished(s) -> failwith s
