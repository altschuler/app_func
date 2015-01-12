module TreeDraw

open TreeDesign

type Coord = int * int

type PostScript = string

let catString s1 s2 = sprintf "%s\n%s" s1 s2

let drawLabel (x, y) label =
  sprintf "newpath
           %d %d moveto
           (%s) dup stringwidth pop 2 div neg 0 rmoveto show" (int x) (int y) label

let drawLine (x1, y1) (x2, y2) =
  let (x1', y1', x2', y2') = (int x1, int y1, int x2, int y2)
  sprintf "newpath
           %d %d moveto
           %d %d lineto
           stroke" x1' y1' x2' y2'

let drawBranch (x1, y1) (x2, y2) =
  let y' = (y1 + y2) / 2.0
  sprintf "%s\n%s\n%s"
    (drawLine (x1, y1) (x1, y'))
    (drawLine (x1, y') (x2, y'))
    (drawLine (x2, y') (x2, y2))

let drawCurve (x1, y1) (x2, y2) =
  let (x1', y1', x2', y2') = (int x1, int y1, int x2, int y2)
  sprintf "newpath
           %d %d moveto
           %d %d %d %d %d %d curveto
           stroke" x1' y1' x1' y2' x2' y1' x2' y2'

let makeConfig (tw, th) ps =
  sprintf "%%!
           /PageSize [ %d %d ]
           1 1 scale
           %d %d translate

           /Times-Roman findfont 12 scalefont setfont

           %s
           showpage" tw 2000 tw th ps

let dimensions tree extent =
  let f (ml, mr) (l, r) = (min ml l, max mr r)
  let maxExt = List.fold f (0.0, 0.0) extent
  let w = (snd maxExt) - (fst maxExt)
  let h = List.length extent
  let (sw, sh) = (30.0, 45)
  (int (w * sw), sh * List.length extent)

let drawTree (tree, extent) =
  let scale = 40.0
  let nh = 10.0
  let size = dimensions tree extent
  let lineStyle = drawCurve

  let rec drawTree' ox depth (Node((label, x), subtrees)) =
    let (px, py) = ((ox + x) * scale, -depth * scale)

    let nodeString = drawLabel (px, py) label

    let f str (Node((label, x'), subtrees) as n) =
      let childString = drawTree' (ox + x) (depth + 1.0) n
      let (cx, ch) = ((ox + x + x') * scale, -(depth + 1.0) * scale)
      let lineString = lineStyle (px, py - 3.0) (cx, ch + nh)
      catString str (catString childString lineString)

    let childrenString = List.fold f "" subtrees

    catString childrenString nodeString

  makeConfig size (drawTree' 0.0 0.0 tree)
