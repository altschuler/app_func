namespace Nim

module Driver =

  open System
  open System.Net
  open System.Threading
  open System.Windows.Forms
  open System.Drawing

  open Nim.UI
  open Nim.GUI
  open Nim.Exceptions
  open Nim.Core
  open Nim.Service
  open Nim.Utils


  // types

  // asynchronous event queue kindly provided by Don Syme
  type AsyncEventQueue<'T>() =
    let mutable cont = None
    let queue = System.Collections.Generic.Queue<'T>()
    let tryTrigger () =
      match queue.Count, cont with
        | _, None -> ()
        | 0, _ -> ()
        | _, Some d ->
          cont <- None
          d (queue.Dequeue())

    let tryListen d =
      if cont.IsSome then invalidOp "multicast not allowed"
      cont <- Some d
      tryTrigger ()

    member x.Post msg = queue.Enqueue msg; tryTrigger()
    member x.Receive() =
      Async.FromContinuations (fun (cont,econt,ccont) ->
          tryListen cont)

  // enumeration of the possible events
  type Event =
    | Load of string
    | Loaded of string
    | Cancel
    | Cancelled
    | Error
    | HumanMove of GameMove
    | ComputerMove


  // variables

  let ev = AsyncEventQueue()

  let gameLoader = GameLoader()

  let loadFn url  = ev.Post (Load url)
  let cancelFn _  = ev.Post Cancel
  let moveFn move = ev.Post (HumanMove move)
  let compFn _    = ev.Post ComputerMove


  // functions

  let go (ui : UI) =

    let tryMove (game:Game) move =
      try game.Move move
      with
        | InvalidMove(s) ->
          ui.Notify s
          game

    let rec ready (m : string option) = async {
      ui.Render (Ready m)

      let! msg = ev.Receive()

      match msg with
        | Load url -> return! loading url
        | _        -> return! ready (Some(sprintf "Unexpected message %A in state Ready" msg))
      }

    and loading (url : string) = async {
      ui.Render (Loading url)

      use ts = new CancellationTokenSource()

      Async.StartWithContinuations (gameLoader.Fetch(url),
                                    (fun str -> ev.Post (Loaded str)),
                                    (fun _ -> ev.Post Error),
                                    (fun _ -> ev.Post Cancelled),
                                    ts.Token)

      let! msg = ev.Receive()

      try
        match msg with
          | Loaded str ->
            try
              let game = new Game(Human, new Board(parseInts str), false)
              return! play game None
            with
              | :? System.FormatException -> raiseParseError url
          | Error   -> return! ready (Some "An error occured while fetching")
          | Cancel  ->
            ts.Cancel()
            return! cancelling ()
          | _       -> return! ready (Some(sprintf "Unexpected message %A in state Loading" msg))
      with
        | ParseError(s) -> return! ready (Some s)
      }

    and cancelling () = async {
      ui.Render Cancelling

      let! msg = ev.Receive()

      match msg with
        | Cancelled
        | Error
        | Loaded _ -> return! ready (Some "Cancelled fetch")
        | _        -> return! ready (Some(sprintf "Unexpected message %A in state Cancelling" msg))
      }

    and play (game:Game) ss = async {
      ui.Render (Playing (game, ss))

      let! msg = ev.Receive()

      match msg with
        | Cancel       -> return! ready None
        | HumanMove m  -> return! play (tryMove game m) None
        | ComputerMove -> return! play (game.ComputerMove()) None // TODO: fix ai
        | _            -> return! play game (Some(sprintf "Unexpected message %A in state Playing" msg))
      }


    // go

    Async.StartImmediate(ready None)
    ui.Go ()
