namespace Nim

module Main =
  open System
  open System.Net
  open System.Threading
  open System.Windows.Forms
  open System.Drawing

  open Nim.GUI
  open Nim.Core
  open Nim.Service
  open Nim.Utils


  // An asynchronous event queue kindly provided by Don Syme
  type AsyncEventQueue<'T>() =
      let mutable cont = None
      let queue = System.Collections.Generic.Queue<'T>()
      let tryTrigger() =
          match queue.Count, cont with
          | _, None -> ()
          | 0, _ -> ()
          | _, Some d ->
              printfn "%A" d
              cont <- None
              d (queue.Dequeue())

      let tryListen(d) =
          if cont.IsSome then invalidOp "multicast not allowed"
          cont <- Some d
          tryTrigger()

      member x.Post msg = queue.Enqueue msg; tryTrigger()
      member x.Receive() =
          Async.FromContinuations (fun (cont,econt,ccont) ->
              tryListen cont)

  // An enumeration of the possible events
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


  let ev = AsyncEventQueue()

  let gui = GUI (
    (fun url  -> ev.Post (LoadGame url)),
    (fun move -> ev.Post (MakeMove move)),
    (fun _    -> ev.Post ComputerMove))
  let gameLoader = GameLoader()

  let rec ready() = async {
    gui.UrlBox.Text <- "http://www2.compute.dtu.dk/~mire/nim.game"
    gui.SetStatus "Hello"

    gui.Disable []

    let! msg = ev.Receive()

    match msg with
      | LoadGame url -> return! loading(url)
      | Clear        -> return! ready()
      | _            -> failwith("ready: unexpected message")
    }

  and loading(url) = async {
    gui.SetStatus (sprintf "Fetching game from %s..." url)

    use ts = new CancellationTokenSource()

    Async.StartWithContinuations (gameLoader.Fetch(url),
                                  (fun html -> ev.Post (Web html)),
                                  (fun _ -> ev.Post Error),
                                  (fun _ -> ev.Post Cancelled),
                                  ts.Token)

    gui.Disable [gui.StartButton]

    let! msg = ev.Receive()

    match msg with
      | Web str ->
          let ints = parseInts str
          let game = new Game(First, new Board(ints), false)
          return! play(game)
      //| Error   -> return! play("Error")
      | Cancel  -> ts.Cancel()
                   return! cancelling()
      | _       -> failwith("loading: unexpected message")
    }

  and cancelling() = async {
    gui.SetStatus "Cancelling"

    gui.Disable [gui.StartButton]

    let! msg = ev.Receive()

    match msg with
      //| Cancelled | Error | Web  _ -> return! finished("Cancelled")
      | _    ->  failwith("cancelling: unexpected message")
    }

  and play(game) = async {
    gui.SetStatus "Playing game"
    gui.Render game

    gui.Disable [gui.StartButton]

    let! msg = ev.Receive()

    match msg with
      | MakeMove m   -> return! play (game.Move m)
      | ComputerMove -> return! play (game.ComputerMove())
      | Clear        -> return! ready()
      | _            -> failwith "play: unexpected message"
    }

  // Start
  Async.StartImmediate (ready())
  Application.Run(gui.Window)
