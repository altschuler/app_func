namespace Nim

module CLI =

  open System

  open Nim.Core
  open Nim.UI


  type CLI(loadFn, cancelFn, moveFn, compFn) =

    // helpers

    let commands = [
      ("load [url]", sprintf "load game from given url (defaults to '%s')" defaultUrl);
      ("cancel", "cancel load");
      ("move <heap> <number>", "take given number of matches from heap");
      ("comp", "computer move")
      ]

    let drawBoard (b : Board) =
      printfn "%s" <| List.fold (fun acc h ->
                                 sprintf "%s\n%s" acc (String.replicate h "x  ")
                                 ) "" b.Heaps

    let drawPrompt () = printf "> "

    let printError s =
      printfn "%s" s
      drawPrompt ()

    // functions

    interface UI with

      member this.Go () =
        while true do
          let input = Console.ReadLine().Split ' '

          match input.[0] with
            | "load"   ->
              if input.Length > 1
              then loadFn input.[1]
              else loadFn "http://www2.compute.dtu.dk/~mire/nim.game"

            | "cancel" -> cancelFn ()

            | "move"   ->
              try moveFn (int input.[1], int input.[2])
              with
                | :? FormatException
                | :? IndexOutOfRangeException -> printError "Wrong arguments, expected two ints"

            | "comp"   -> compFn ()

            | "help"   ->
              printfn "Commands:"
              ignore <| List.map (fun (c, d) -> printfn "\t%20s  -  %s" c d) commands

            | x        -> printError (sprintf "Unknown command '%s'" x)

      member this.Notify s = printfn "%s" s

      member this.Render (state : UIState) =
        match state with
          | Ready ss ->
            match ss with
              | Some s -> printfn "%s" s
              | None   ->
                printfn "Welcome!\nType 'help' for commands"
            drawPrompt ()

          | Loading url ->
            printfn "Fetching %s..." url

          | Cancelling ->
            printfn "Cancelling..."

          | Playing (game, ss) ->
            match ss with
              | Some s -> printfn "%s" s
              | None   -> printfn "Make a move, %A!" game.Turn
            drawBoard game.Board

            // TODO: merge with above msg handling dauda
            if game.Finished
            then printfn "Game finished"

            drawPrompt ()
