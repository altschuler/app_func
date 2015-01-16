namespace Nim

module Main =

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
  type Message =
    | Load of string
    | Web of string
    | Cancel
    | Cancelled
    | Error
    | MakeMove of GameMove
    | ComputerMove


  // variables

  let ev = AsyncEventQueue()

  let ui =
    GUI (
      (fun url  -> ev.Post (Load url)),
      (fun _    -> ev.Post Cancel),
      (fun move -> ev.Post (MakeMove move)),
      (fun _    -> ev.Post ComputerMove))

  let gameLoader = GameLoader()


  // functions

  let rec ready (m : string option) = async {
    ui.Render (Ready m)

    let! msg = ev.Receive()

    match msg with
      | Load url -> return! loading url
      | x        -> failwith (sprintf "ready: unexpected message '%A'" x)
    }

  and loading url = async {
    ui.Render (Loading url)

    use ts = new CancellationTokenSource()

    Async.StartWithContinuations (gameLoader.Fetch(url),
                                  (fun html -> ev.Post (Web html)),
                                  (fun _ -> ev.Post Error),
                                  (fun _ -> ev.Post Cancelled),
                                  ts.Token)

    let! msg = ev.Receive()

    try
      match msg with
        | Web str ->
          try
            let game = new Game(First, new Board(parseInts str), false)
            return! play game
          with
            | :? System.FormatException -> raiseParseError url
        | Error   -> return! ready (Some "An error occured while fetching")
        | Cancel  ->
          ts.Cancel()
          return! cancelling ()
        | x       -> failwith (sprintf "loading: unexpected message '%A'" x)
    with
      | ParseError(s) -> return! ready (Some s)
    }

  and cancelling () = async {
    ui.Render Cancelling

    let! msg = ev.Receive()

    match msg with
      | Cancelled
      | Error
      | Web _ -> return! ready (Some "Cancelled fetch")
      | x     -> failwith (sprintf "cancelling: unexpected message '%A'" x)
    }

  and play (game:Game) = async {
    ui.Render (Playing game)

    try
      let! msg = ev.Receive()

      match msg with
        | Cancel       -> return! ready None
        | MakeMove m   -> return! play (game.Move m)
        | ComputerMove -> return! play (game.ComputerMove())
        | x            -> failwith (sprintf "play: unexpected message '%A'" x)
    with
      | InvalidMove(s) ->
        ui.Notify s
        ignore <| play game
    }


  // go

  Async.StartImmediate(ready None)
  Application.Run(ui.Window)
