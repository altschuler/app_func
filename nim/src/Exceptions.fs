namespace Nim

module Exceptions =

  exception InvalidMove of string
  exception GameFinished of string
  exception ParseError of string

  let raiseInvalidMove (heap:int) =
    raise (InvalidMove(sprintf "Not enough matches in heap #%d" heap))

  let raiseGameFinished () = raise (GameFinished("Game is already finished"))

  let raiseParseError (url:string) =
    raise (ParseError(sprintf "Could not parse '%s'" url))
