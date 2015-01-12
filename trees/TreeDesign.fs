module TreeDesign

type Tree<'a> = Node of 'a * ('a Tree list)

let movetree (Node((label, x), subtrees), x') =
  Node((label, x+x'), subtrees)

type Extent = (float * float) list

let moveextent (e, x) = List.map (fun (p,q) -> (p+x, q+x)) e

let rec merge rs ls =
  match rs, ls with
  | [], qs -> qs
  | ps, [] -> ps
  | (p, _) :: ps, (_, q) :: qs -> (p, q) :: (merge ps qs)

let mergelist es = List.fold merge [] es

let rec fit (rs:Extent) (ls:Extent) =
  match rs, ls with
  | (_, p) :: ps, (q, _) :: qs -> max (fit ps qs) (p - q + 1.0)
  | _, _ -> 0.0

let fitlistl es =
  let rec fitlistl' acc = function
    | [] -> []
    | (e::es) ->
      let x = fit acc e
      x :: fitlistl' (merge acc (moveextent (e,x))) es
  fitlistl' [] es

let fitlistr es =
  let rec fitlistr' acc = function
    | [] -> []
    | (e::es) ->
      let x = -(fit e acc)
      x :: fitlistr' (merge (moveextent (e,x)) acc) es
  List.rev (fitlistr' [] (List.rev es))

let mean (x, y) = (x + y) / 2.0

let fitlist es = List.map mean (List.zip (fitlistl es) (fitlistr es))

let design tree =
  let rec design' (Node(label, subtrees)) =
    let (trees, extents) = List.unzip (List.map design' subtrees)
    let positions        = fitlist extents
    let ptrees           = List.map movetree (List.zip trees positions)
    let pextents         = List.map moveextent (List.zip extents positions)
    let resultextent     = (0.0, 0.0) :: mergelist pextents
    let resulttree       = Node((label, 0.0), ptrees)
    (resulttree, resultextent)

  design' tree
