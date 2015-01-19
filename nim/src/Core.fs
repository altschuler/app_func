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

  type Player = Human | Computer

  type Game(turn : Player, board : Board, didTaunt : bool, didTauntThisTurn : bool) =

    member private this.NimSum = List.reduce (^^^)

    member this.NextTurn () =
      match this.Turn with
        | Human -> Computer
        | Computer -> Human

    member this.Turn = turn

    member this.Board = board

    member this.DidTaunt = didTaunt

    member this.DidTauntThisTurn = didTauntThisTurn

    member this.CheckTaunt () =
      if not didTaunt
        && (not didTauntThisTurn)
        && this.Turn = Human
        && this.NimSum this.Board.Heaps = 0
      then new Game(this.Turn, this.Board, true, true)
      else new Game(this.Turn, this.Board, this.DidTaunt, false)

    member this.Winner () =
      match List.sum this.Board.Heaps with
        | 0 -> Some (this.NextTurn ())
        | _ -> None

    member this.Move ((heap, number) : GameMove) : Game =
      if number <= 0
        then raise (InvalidMove "You must remove something")

      new Game(this.NextTurn (), (this.Board.Take heap number), this.DidTaunt, this.DidTauntThisTurn)

    member this.ComputerMove() : Game =
      let maxIndex = fst << List.maxBy snd << List.mapi (fun i x -> i, x)

      match this.NimSum this.Board.Heaps with
        | 0 -> // loosing
          this.Move(maxIndex this.Board.Heaps, 1)
        | m -> // winning
          let rec findMove idx = function
            | h::hs ->
              let ns = this.NimSum [m; h]
              if ns < h then (idx, h - ns) else findMove (idx + 1) hs
            | _ -> failwith "Mathematical!" // theoretically unreachable

          this.Move <| findMove 0 this.Board.Heaps

  // helpers

  let newGame () : Game = new Game(Human, new Board([1; 3; 5; 7]), false, false)

  let printBoard (b : Board) =
    List.mapi (fun i h ->
               let spaces = 3 * ((List.length b.Heaps) - i)
               printfn "%s%s" (String.replicate spaces " ") (String.replicate h "x  ")
             ) b.Heaps |> ignore
