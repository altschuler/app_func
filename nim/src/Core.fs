namespace Nim

module Core =

  open Nim.Utils


  // types

  type Heap = int

  type Board(heaps : Heap list) =

    member this.Heaps = heaps

    member this.Take (heap:int) (number:int) : Board =
      new Board(setNth heaps heap ((List.nth heaps heap) - number))


  type Player = First | Second

  type Game(turn : Player, board : Board) =

    member this.Turn = turn

    member this.Board = board

    member this.Move (heap:int) (number:int) : Game =
      printfn "%A player: Took %d from heap #%d" turn number heap

      let newTurn = match turn with
        | First -> Second
        | Second -> First

      new Game(newTurn, (this.Board.Take heap number))


  // helpers

  let newGame () : Game = new Game(First, new Board([1; 3; 5; 7]))

  let printBoard (b : Board) =
    List.mapi (fun i h ->
               let spaces = 3 * ((List.length b.Heaps) - i)
               printfn "%s%s" (String.replicate spaces " ") (String.replicate h "x  ")
             ) b.Heaps |> ignore
