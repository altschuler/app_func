namespace Nim

module UI =

  open Nim.Core

  type UIState =
    | Ready of string option
    | Loading of string
    | Cancelling
    | Playing of Game
