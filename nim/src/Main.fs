namespace Nim

module Main =

  open System
  open System.Net
  open System.Threading
  open System.Windows.Forms
  open System.Drawing

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
    | Start of string
    | Clear
    | Cancel
    | Web of string
    | Error
    | Cancelled
    | MakeMove of GameMove
    | ComputerMove
    | LoadGame of string


  // variables

  let ev = AsyncEventQueue()

  let gui =
    GUI (
      (fun url  -> ev.Post (LoadGame url)),
      (fun move -> ev.Post (MakeMove move)),
      (fun _    -> ev.Post ComputerMove))

  let gameLoader = GameLoader()


  // functions

  let rec ready (m : string option) = async {
    match m with
      | Some s -> gui.SetStatus s
      | None   ->
        gui.SetStatus "Hello"
        gui.UrlBox.Text <- "http://www2.compute.dtu.dk/~mire/nim.game"

    gui.Disable []

    let! msg = ev.Receive()

    match msg with
      | LoadGame url -> return! loading url
      | Clear        -> return! ready None
      | x            -> failwith (sprintf "ready: unexpected message '%A'" x)
    }

  and loading url = async {
    gui.SetStatus (sprintf "Fetching game from %s..." url)

    gui.Disable [gui.LoadButton; gui.MoveButton; gui.CompButton]

    use ts = new CancellationTokenSource()

    Async.StartWithContinuations (gameLoader.Fetch(url),
                                  (fun html -> ev.Post (Web html)),
                                  (fun _ -> ev.Post Error),
                                  (fun _ -> ev.Post Cancelled),
                                  ts.Token)

    let! msg = ev.Receive()

    match msg with
      | Web str ->
        let game = new Game(First, new Board(parseInts str), false)
        return! play game
      //| Error   -> return! play("Error")
      | Cancel  ->
        ts.Cancel()
        return! cancelling ()
      | x       -> failwith (sprintf "loading: unexpected message '%A'" x)
    }

  and cancelling () = async {
    gui.SetStatus "Cancelling"

    gui.Disable [gui.LoadButton; gui.MoveButton]

    let! msg = ev.Receive()

    match msg with
      //| Cancelled | Error | Web  _ -> return! finished("Cancelled")
      | _    ->  failwith("cancelling: unexpected message")
    }

  and play (game:Game) = async {
    gui.Render game
    gui.Disable [gui.LoadButton]

    if game.Finished
    then
      gui.SetStatus "Game finished"
      gui.Disable [gui.MoveButton; gui.CompButton]
    else
      gui.SetStatus "Playing game"

    try
      let! msg = ev.Receive()

      match msg with
        | MakeMove m   -> return! play (game.Move m)
        | ComputerMove -> return! play (game.ComputerMove())
        | Clear        -> return! ready None
        | x            -> failwith (sprintf "play: unexpected message '%A'" x)
    with
      | InvalidMove(s) ->
        gui.SetStatus s
        ignore <| play game
    }


  // go

  Async.StartImmediate(ready None)
  Application.Run(gui.Window)
