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
        AutoSize = true,
        Size = Size(640, 560))

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
      new GroupBox(
        Enabled = false,
        AutoSize = true,
        Location = Point(25, 150))

    let compButton =
      new Button(
        Location = Point(525, 100),
        MinimumSize = Size(50, 25),
        MaximumSize = Size(100, 25),
        Text = "Computer")

    // helpers

    let matchImage = Image.FromFile("images/doge_50.png")

    let setStatus (s : string) = status.Text <- s

    let buttons : Button list = [
      loadButton;
      cancelButton;
      compButton
      ]

    let disable (bs : Button list) =
      ignore <| List.map (fun (b : Button) ->
                          b.Enabled <- (not (List.exists ((=) b) bs))
                          ) buttons

    let prompt title body =
      ignore <| MessageBox.Show(
        body,
        title,
        MessageBoxButtons.OK,
        MessageBoxIcon.Warning)

    let drawBoard (b : Board) =
      board.Controls.Clear()

      ignore <| List.mapi (fun y h ->
                 List.mapi (fun _ x ->
                            let cb =
                              new Button(
                                BackgroundImage = matchImage,
                                Location = Point(5 + x * 55, 10 + y * 55),
                                MinimumSize = Size(50, 50),
                                MaximumSize = Size(50, 50))

                            cb.Click.Add (fun _ -> moveFn (y, h - x))

                            board.Controls.Add(cb)) [ 0 .. h - 1 ]
                 ) b.Heaps

    do
      // listeners
      loadButton.Click.Add   (fun _ -> loadFn urlBox.Text)
      cancelButton.Click.Add (fun _ -> cancelFn ())
      compButton.Click.Add   (fun _ -> compFn ())

      // finish
      window.Controls.Add status
      window.Controls.Add label
      window.Controls.Add urlBox
      window.Controls.Add cancelButton
      window.Controls.Add loadButton
      window.Controls.Add board
      window.Controls.Add compButton

    // functions

    interface UI with

      member this.Go () = Application.Run window

      member this.Render (state : UIState) =
        match state with
          | Ready ss ->
            match ss with
              | Some s -> setStatus s
              | None   ->
                setStatus "Welcome!"
                urlBox.Text <- defaultUrl
            disable [cancelButton; compButton]

          | Loading url ->
            setStatus (sprintf "Fetching %s..." url)
            disable [loadButton; compButton]

          | Cancelling ->
            setStatus "Cancelling..."
            disable [loadButton; cancelButton; compButton]

          | Playing (game, ss) ->
            if game.DidTauntThisTurn then prompt "Computer says" tauntMsg

            match ss with
              | Some s -> setStatus s
              | None   -> setStatus (sprintf "Make a move, %A!" game.Turn)

            board.Enabled <- game.Turn = Human
            compButton.Enabled <- game.Turn = Computer

            drawBoard game.Board

            disable [loadButton]

          | Finished player ->
            let msg = match player with
              | Human    -> "You won :)"
              | Computer -> "You lost :'("
            prompt "Game finished" msg

            disable [cancelButton; compButton]

            ignore <| board.Controls.Clear ()

            setStatus "Game finished"
