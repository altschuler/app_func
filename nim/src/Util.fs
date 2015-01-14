namespace Nim

module Util =

  open System

  let rec printGame = function
    | [] -> printfn ""
    | h :: hs ->
      printfn "%A" (List.map (fun h' -> "x") h)
      printGame hs
