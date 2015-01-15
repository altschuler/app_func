namespace Nim

module GUI =

  open System.Windows.Forms
  open System.Drawing
  open Nim.Core

  type GUI(startFn, moveFn, compFn) =

    let window =
      new Form(
        Text = "Nim",
        Size = Size(640, 480))

    let label =
      new Label(
        Text = "Enter URL:",
        Size = Size(100, 100),
        AutoSize = true,
        Location = Point(25, 25))

    let urlBox =
      new TextBox(
        Location = Point(25, 50),
        Size = Size(400, 50))

    let status =
      new Label(
        Text = "Welcome",
        Size = Size(300, 50),
        AutoSize = true,
        Location = Point(25, 100))

    let startButton =
      new Button(
        Location = Point(450, 50),
        MinimumSize = Size(50, 25),
        MaximumSize = Size(50, 25),
        Text = "Get")

    let board =
      new Label(
        Text = "",
        Size = Size(300, 300),
        AutoSize = true,
        Location = Point(25, 150))

    let compButton =
      new Button(
        Location = Point(525, 100),
        MinimumSize = Size(50, 25),
        MaximumSize = Size(100, 25),
        Text = "Computer")

    let moveButton =
      new Button(
        Location = Point(525, 50),
        MinimumSize = Size(50, 25),
        MaximumSize = Size(50, 25),
        Text = "Move")

    do
      // listeners
      startButton.Click.Add (fun _ -> startFn urlBox.Text)
      moveButton.Click.Add (fun _ -> moveFn (0, 1))
      compButton.Click.Add (fun _ -> compFn ())

      // finish
      window.Controls.Add(status)
      window.Controls.Add(label)
      window.Controls.Add(urlBox)
      window.Controls.Add(startButton)
      window.Controls.Add(board)
      window.Controls.Add(moveButton)
      window.Controls.Add(compButton)

    // components TODO: remove
    member this.Window = window
    member this.Label = label
    member this.UrlBox = urlBox
    member this.Status = status
    member this.StartButton = startButton
    member this.MoveButton = moveButton
    member this.CompButton = compButton

    // functions
    member this.Disable bs =
      for b in [startButton] do
        b.Enabled  <- true
      for (b:Button) in bs do
        b.Enabled  <- false

    member this.SetStatus s = this.Status.Text <- s

    member this.Render (game:Game) =
      board.Text <- List.fold (fun acc h ->
                               sprintf "%s\n%s" acc (String.replicate h "x  ")
                               ) "" game.Board.Heaps
