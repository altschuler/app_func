module TreeDraw

open TreeDesign

type Coord = int * int

type PostScript = string

let catString s1 s2 = sprintf "%s\n%s" s1 s2

let drawLabel (x, y) label =
  sprintf "/Times-Roman findfont
           12 scalefont
           setfont
           newpath
           %d %d moveto
           (%s) dup stringwidth pop 2 div neg 0 rmoveto show" (int x) (int y) label

let drawLine (x1, y1) (x2, y2) =
  sprintf "newpath
           %d %d moveto
           %d %d lineto
           stroke" (int x1) (int y1) (int x2) (int y2)

let makeConfig ps =
  sprintf "%%!
           1 1 scale
           %s
           showpage" ps

let drawTree tree =
  let scale = 50.0

  let rec drawTree' (ox, oy) depth (Node((label, x), subtrees)) =
    let (px, py) as nodePos = ((ox + x) * scale, (depth + oy) * scale)

    let nodeString = drawLabel nodePos label

    let f str (Node((label, x'), subtrees) as n) =
      let childString = drawTree' (ox + x, oy) (depth + 1.0) n
      let childPos = ((ox + x + x') * scale, (depth + oy + 1.0) * scale)
      let lineString = drawLine (px, py) childPos
      catString str (catString childString lineString)

    let childrenString = List.fold f "" subtrees

    catString childrenString nodeString

  drawTree' (5.0, 1.0) 0.0 tree
