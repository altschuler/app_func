namespace Nim

module GUI =

  open System.Windows.Forms
  open System.Drawing

  type GUI(startFn, moveFn) =

    let window =
      new Form(
        Text = "Nim",
        Size = Size(535, 225))

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

    let output =
      new TextBox(
        Location = Point(25, 100),
      Size = Size(400, 50))

    let startButton =
      new Button(
        Location = Point(450, 50),
        MinimumSize = Size(50, 25),
        MaximumSize = Size(50, 25),
        Text = "Get")

    let moveButton =
      new Button(
        Location = Point(450, 120),
        MinimumSize = Size(50, 25),
        MaximumSize = Size(50, 25),
        Text = "Move")

    do
      // listeners
      startButton.Click.Add (fun _ -> startFn urlBox.Text)
      moveButton.Click.Add (fun _ -> moveFn (0, 1))

      // finish
      window.Controls.Add(output)
      window.Controls.Add(label)
      window.Controls.Add(urlBox)
      window.Controls.Add(startButton)
      window.Controls.Add(moveButton)

    // components
    member this.Window = window
    member this.Label = label
    member this.UrlBox = urlBox
    member this.Output = output
    member this.StartButton = startButton
    member this.MoveButton = moveButton
    member this.Disable bs =
      for b in [startButton] do
        b.Enabled  <- true
      for (b:Button) in bs do
        b.Enabled  <- false
