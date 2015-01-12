module TreeDraw

open TreeDesign

type Coord = int * int

type PostScript = string

let catString s1 s2 = sprintf "%s\n%s" s1 s2

let drawLabel s (x, y) label =
  let cmd = sprintf "/Times-Roman findfont
                     12 scalefont
                     setfont
                     newpath
                     %d %d moveto
                     (%s) show" (int x) (int y) label
  catString s cmd

let drawLine s (x1, y1) (x2, y2) =
  let cmd = sprintf "newpath
                     %d %d moveto
                     %d %d lineto
                     stroke" (int x1) (int y1) (int x2) (int y2)
  catString s cmd


let makeConfig ps =
  sprintf "%%!
           1 1 scale
           %s
           showpage" ps

let drawNode x y label = drawLabel x y label

let drawTree tree =
  let scale = 50.0

  let rec drawTree' s (ox, oy) depth (Node((label, x), subtrees)) =
    let (px, py) as nodePos = ((ox + x) * scale, (depth + oy) * scale)
    let s' = drawLabel s nodePos label
    List.fold (fun str (Node((label, x'), subtrees) as n) ->
               let s1 = drawTree' str (ox + x, oy) (depth + 1.0) n
               let childPos = ((ox + x + x') * scale, (depth + oy + 1.0) * scale)
               drawLine s1 (px, py) childPos
               ) s' subtrees

  drawTree' "" (5.0, 1.0) 0.0 tree
