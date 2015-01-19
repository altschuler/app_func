namespace Nim

module UI =

  open Nim.Core

  type UIState =
    | Ready of string option
    | Loading of string
    | Cancelling
    | Playing of Game * string option

  type UI =

    abstract member Go : unit -> unit

    abstract member ShowWinner : Player -> unit

    abstract member Render : UIState -> unit


  let tauntMsg = "You're gonna lose!"

  let defaultUrl = "http://www2.compute.dtu.dk/~mire/nim.game"
