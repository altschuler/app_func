namespace Nim

module Main =

  open Nim.Core
  open Nim.Utils
  open Nim.Exceptions
  open Nim.Service

  [<EntryPoint>]
  let main argv =

    let play (s:string) =
      let g = new Game(First, new Board(parseInts s), false)

      let taunt (g:Game) =
        let (taunt, g') = g.Taunt()
        if taunt then printfn "U gon die" // TODO: taunt with gui
        g'

      let g0 = newGame ()

      printBoard g0.Board

      let g1 = g0.Move (0, 1)
      let g2 = taunt g1
      let g3 = g2.ComputerMove()
      let g4 = taunt g3

      printBoard g4.Board

      printfn "finished: %b" g4.Finished

      let g5 = g4.Move(3, 3).Move(2, 5)//.Move 3 5

      printfn "finished: %b, winner: %A" g5.Finished g5.Turn
      ignore <| g5.Move(1, 1)

    try
      fetch "http://www2.compute.dtu.dk/~mire/nim.game" (play)

    with
      | InvalidMove(s) -> failwith s
      | GameFinished(s) -> failwith s

    0
