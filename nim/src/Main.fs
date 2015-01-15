namespace Nim

module Main =
  open System
  open System.Net
  open System.Threading
  open System.Windows.Forms
  open System.Drawing

  open Nim.GUI
  open Nim.Core


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
    | Start of string | Clear | Cancel | Web of string | Error | Cancelled
    | MakeMove of GameMove


  let ev = AsyncEventQueue()

  let gui = GUI ((fun url -> ev.Post (Start url)), (fun move -> ev.Post (MakeMove move)))
  let game = newGame()

  let rec ready() = async {
    gui.UrlBox.Text <- "http://"
    gui.Output.Text <- ""

    gui.Disable []

    let! msg = ev.Receive()

    match msg with
      | Start url  -> return! loading(url)
      | Clear      -> return! ready()
      | MakeMove m -> printBoard ((game.Move m).Board)
      | _          -> failwith("ready: unexpected message")
  }

  and loading(url) =
    async {gui.Output.Text <- "Downloading"
           use ts = new CancellationTokenSource()
            // start the load
           Async.StartWithContinuations
               (async { let webCl = new WebClient()
                        let! html = webCl.AsyncDownloadString(Uri url)
                        return html },
                (fun html -> ev.Post (Web html)),
                (fun _ -> ev.Post Error),
                (fun _ -> ev.Post Cancelled),
                ts.Token)

           gui.Disable [gui.StartButton]
           let! msg = ev.Receive()
           match msg with
           | Web html ->
               let ans = "Length = " + String.Format("{0:D}",html.Length)
               return! finished(ans)
           | Error   -> return! finished("Error")
           | Cancel  -> ts.Cancel()
                        return! cancelling()
           | _       -> failwith("loading: unexpected message")}

  and cancelling() =
    async {gui.Output.Text <- "Cancelling"

           gui.Disable [gui.StartButton]
           let! msg = ev.Receive()
           match msg with
           | Cancelled | Error | Web  _ ->
                     return! finished("Cancelled")
           | _    ->  failwith("cancelling: unexpected message")}

  and finished(s) =
    async {gui.Output.Text <- s

           gui.Disable [gui.StartButton]
           let! msg = ev.Receive()
           match msg with
           | Clear -> return! ready()
           | _     ->  failwith("finished: unexpected message")}

  // Initialization
  // window.Controls.Add urlBox
  // window.Controls.Add ansBox
  // window.Controls.Add startButton
  // window.Controls.Add clearButton
  // window.Controls.Add cancelButton
  // startButton.Click.Add
  // clearButton.Click.Add (fun _ -> ev.Post Clear)
  // cancelButton.Click.Add (fun _ -> ev.Post Cancel)

  // Start
  Async.StartImmediate (ready())
  Application.Run(gui.Window)


  // namespace Nim

  // module Main =

  //   //open Nim.Core
  //   //open Nim.Utils
  //   open Nim.Exceptions
  //   //open Nim.Service
  //   //open Nim.GUI
  //   open Nim.Flow

  //   [<EntryPoint>]
  //   let main argv =
  //     startFlow()
  //     // let play (s:string) =
  //     //   let g = new Game(First, new Board(parseInts s), false)

  //     //   let taunt (g:Game) =
  //     //     let (taunt, g') = g.Taunt()
  //     //     if taunt then printfn "U gon die" // TODO: taunt with gui
  //     //     g'

  //     //   let g0 = newGame ()

  //     //   printBoard g0.Board

  //     //   let g1 = g0.Move (0, 1)
  //     //   let g2 = taunt g1
  //     //   let g3 = g2.ComputerMove()
  //     //   let g4 = taunt g3

  //     //   printBoard g4.Board

  //     //   printfn "finished: %b" g4.Finished

  //     //   let g5 = g4.Move(3, 3).Move(2, 5)//.Move 3 5

  //     //   printfn "finished: %b, winner: %A" g5.Finished g5.Turn
  //     //   ignore <| g5.Move(1, 1)

  //     // try
  //     //   //fetch "http://www2.compute.dtu.dk/~mire/nim.game" (play)
  //     //   Application.Run(window)

  //     // with
  //     //   | InvalidMove(s)
  //     //   | GameFinished(s)
  //     //   | ParseError(s) -> failwith s

  //     0
