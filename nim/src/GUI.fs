namespace Nim

module GUI =

  open System.Windows.Forms
  open System.Drawing


  // components

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

  let startButton =
    new Button(
      Location = Point(450, 50),
      MinimumSize = Size(50, 25),
      MaximumSize = Size(50, 25),
      Text = "Get")


  // listeners

  startButton.Click.Add (fun _ -> printfn "Getzz")


  // finish

  window.Controls.Add(label)
  window.Controls.Add(urlBox)
  window.Controls.Add(startButton)
