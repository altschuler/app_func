namespace Nim

module Core =

  type Match = Match of unit

  type Heap  = Match list

  type Game  = Heap list

  let m = Match ()


  let createGame () : Game =
    let size = 4

    let rec createGame' = function
      | n when n > 0 ->
          [List.replicate (2 * (size - n) + 1) m] @ createGame' (n - 1)
      | _            -> []

    createGame' size
