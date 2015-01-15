namespace Nim

module Core =

  open Nim.Utils
  open Nim.Exceptions


  // types

  type Heap = int

  type Board(heaps : Heap list) =

    member this.Heaps = heaps

    member this.Take (heap:int) (number:int) : Board =
      if (List.nth heaps heap) - number < 0 then raiseInvalidMove heap
      new Board(setNth heaps heap ((List.nth heaps heap) - number))

  type GameMove = int * int

  type Player = First | Second

  type Game(turn : Player, board : Board, didWarn : bool) =

    member private this.NimSum = List.reduce (^^^)

    member this.Turn = turn

    member this.Board = board

    member this.DidWarn = didWarn

    member this.Warn() =
      if not didWarn
        && this.Turn = First
        && this.NimSum this.Board.Heaps = 0
      then
        printfn "U gon die" // TODO: warn with gui
        new Game(this.Turn, this.Board, true)
      else
        this

    member this.Finished = (List.sum this.Board.Heaps) = 0

    member this.Move ((heap, number) : GameMove) : Game =
      if this.Finished then raiseGameFinished ()

      let newTurn = match turn with
        | First -> Second
        | Second -> First

      new Game(newTurn, (this.Board.Take heap number), this.DidWarn)

    member this.ComputerMove() : Game =
      let m = this.NimSum this.Board.Heaps
      match m with
        | 0 ->
          let f (big, idx) heap = (max heap big, idx + 1)
          let (_, bigIdx) = List.fold f (0, 0) this.Board.Heaps
          this.Move(bigIdx - 1, 1) // idx is 1-based
        | _ ->
          let rec findMove idx = function
            | h::hs ->
              let ns = this.NimSum [m; h]
              if ns < h then (idx, h - ns) else findMove (idx + 1) hs
            | _ -> failwith "Mathematical!" // theoretically unreachable

          this.Move <| findMove 0 this.Board.Heaps

  // helpers

  let newGame () : Game = new Game(First, new Board([1; 3; 5; 7]), false)

  let printBoard (b : Board) =
    List.mapi (fun i h ->
               let spaces = 3 * ((List.length b.Heaps) - i)
               printfn "%s%s" (String.replicate spaces " ") (String.replicate h "x  ")
             ) b.Heaps |> ignore
