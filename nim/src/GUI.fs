namespace Nim

module GUI =

  open System.Windows.Forms
  open System.Drawing

  open Nim.Core
  open Nim.UI

  type GUI(loadFn, cancelFn, moveFn, compFn) =

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

    let loadButton =
      new Button(
        Location = Point(450, 50),
        MinimumSize = Size(50, 25),
        MaximumSize = Size(50, 25),
        Text = "Load")

    let cancelButton =
      new Button(
        Location = Point(525, 50),
        MinimumSize = Size(50, 25),
        MaximumSize = Size(100, 25),
        Text = "Cancel")

    let board =
      new Label(
        Text = "",
        Size = Size(300, 300),
        AutoSize = true,
        Location = Point(25, 150))

    let moveButton =
      new Button(
        Location = Point(450, 100),
        MinimumSize = Size(50, 25),
        MaximumSize = Size(50, 25),
        Text = "Move")

    let compButton =
      new Button(
        Location = Point(525, 100),
        MinimumSize = Size(50, 25),
        MaximumSize = Size(100, 25),
        Text = "Computer")

    // helpers

    let setStatus s = status.Text <- s

    let buttons = [loadButton; cancelButton; moveButton; compButton]

    let disable (bs : Button list) =
      for b in buttons do
        b.Enabled <- true
      for b in bs do
        b.Enabled <- false

    let drawBoard (b : Board) =
      board.Text <- List.fold (fun acc h ->
                               sprintf "%s\n%s" acc (String.replicate h "x  ")
                               ) "" b.Heaps

    do
      // listeners
      loadButton.Click.Add   (fun _ -> loadFn urlBox.Text)
      cancelButton.Click.Add (fun _ -> cancelFn ())
      moveButton.Click.Add   (fun _ -> moveFn (0, 1))
      compButton.Click.Add   (fun _ -> compFn ())

      // finish
      window.Controls.Add status
      window.Controls.Add label
      window.Controls.Add urlBox
      window.Controls.Add cancelButton
      window.Controls.Add loadButton
      window.Controls.Add board
      window.Controls.Add moveButton
      window.Controls.Add compButton

    // functions

    interface UI with

      member this.Go () = Application.Run window

      member this.Notify s = setStatus s

      member this.Render (state : UIState) =
        match state with
          | Ready ss ->
            match ss with
              | Some s -> setStatus s
              | None   ->
                setStatus "Welcome!"
                urlBox.Text <- defaultUrl
            disable [cancelButton; moveButton; compButton]

          | Loading url ->
            setStatus (sprintf "Fetching %s..." url)
            disable [loadButton; moveButton; compButton]

          | Cancelling ->
            setStatus "Cancelling..."
            disable [loadButton; cancelButton; moveButton; compButton]

          | Playing (game, ss) ->
            match ss with
              | Some s -> setStatus s
              | None   ->
                setStatus (sprintf "Make a move, %A!" game.Turn)
            drawBoard game.Board
            disable [loadButton]

            // TODO: merge with above msg handling dauda
            if game.Finished
            then
              setStatus "Game finished"
              disable [moveButton; compButton]
