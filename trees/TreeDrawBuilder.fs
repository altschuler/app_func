module TreeDraw

open System.Text
open TreeDesign

let drawLabel (sb:StringBuilder) (x, y) (label:string) =
  ignore <| sb.AppendLine "newpath"
  ignore <| sb.Append (int x)
  ignore <| sb.Append " "
  ignore <| sb.Append (int y)
  ignore <| sb.AppendLine " moveto"
  ignore <| sb.Append "("
  ignore <| sb.Append label
  ignore <| sb.AppendLine ") dup stringwidth pop 2 div neg 0 rmoveto show"

let drawLine (sb:StringBuilder) (x1, y1) (x2, y2) =
  let (x1', y1', x2', y2') = (int x1, int y1, int x2, int y2)
  ignore <| sb.AppendLine "newpath"

  ignore <| sb.Append x1'
  ignore <| sb.Append " "
  ignore <| sb.Append y1'
  ignore <| sb.AppendLine " moveto"

  ignore <| sb.Append x2'
  ignore <| sb.Append " "
  ignore <| sb.Append y2'
  ignore <| sb.AppendLine " lineto"
  ignore <| sb.AppendLine "stroke"

let drawBranch (sb:StringBuilder) (x1, y1) (x2, y2) =
  let y' = (y1 + y2) / 2.0
  ignore <| drawLine sb (x1, y1) (x1, y')
  ignore <| drawLine sb (x1, y') (x2, y')
  ignore <| drawLine sb (x2, y') (x2, y2)

let drawCurve (sb:StringBuilder) (x1, y1) (x2, y2) =
  let (x1', y1', x2', y2') = (int x1, int y1, int x2, int y2)

  ignore <| sb.AppendLine "newpath"

  ignore <| sb.Append x1'
  ignore <| sb.Append " "
  ignore <| sb.Append y1'
  ignore <| sb.AppendLine " moveto"

  ignore <| sb.Append x1'
  ignore <| sb.Append " "
  ignore <| sb.Append y2'
  ignore <| sb.Append " "
  ignore <| sb.Append x2'
  ignore <| sb.Append " "
  ignore <| sb.Append y1'
  ignore <| sb.Append " "
  ignore <| sb.Append x2'
  ignore <| sb.Append " "
  ignore <| sb.Append y2'
  ignore <| sb.AppendLine " curveto"
  ignore <| sb.AppendLine "stroke"

let initSb (sb:StringBuilder) (tw:int, th:int) =
  ignore <| sb.AppendLine "%%!
                 /Times-Roman findfont 10 scalefont setfont
                 1 1 scale"
  ignore <| sb.Append tw
  ignore <| sb.Append " "
  ignore <| sb.Append th
  ignore <| sb.AppendLine " translate"

let endSb (sb:StringBuilder) =
  ignore <| sb.AppendLine "showpage"

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
  let sb = StringBuilder()
  let lineStyle = drawCurve

  ignore <| initSb sb size

  let rec drawTree' ox depth (Node((label, x), subtrees)) =
    let (px, py) = ((ox + x) * scale, -depth * scale)

    ignore <| drawLabel sb (px, py) label

    let f (Node((label, x'), subtrees) as n) =
      ignore <| drawTree' (ox + x) (depth + 1.0) n
      let (cx, ch) = ((ox + x + x') * scale, -(depth + 1.0) * scale)
      ignore <| lineStyle sb (px, py - 3.0) (cx, ch + nh)

    List.iter f subtrees

  ignore <| drawTree' 0.0 0.0 tree

  ignore <| endSb sb

  sb.ToString()
