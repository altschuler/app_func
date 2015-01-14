namespace Nim

module Core =

  type Match = Match of unit

  type Heap  = Match list



  type Board() =
    let mutable heaps = []

    let size = 4

    let rec createBoard = function
      | n when n > 0 ->
        let numMatches = (2 * (size - n) + 1)
        [List.replicate numMatches (Match ())] @ createBoard (n - 1)
      | _ -> []

    let rec printBoard = function
      | [] -> printfn ""
      | h :: hs ->
        printfn "%A" (List.map (fun h' -> "x") h)
        printBoard hs

    do heaps <- createBoard size

    member this.Size() = size

    member this.Print() = printBoard heaps


  type Game() =
    do printfn "Good luck!"

    member this.Board() = new Board()
