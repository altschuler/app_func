module TransformAST

open TreeDesign
open AST

let n label children = [ Node(label, children) ]

let rec tExp = function
  | Int i -> n (sprintf "IntVal '%d'" i) []
  | Bool b -> n (sprintf "BoolVal '%A'" b) []
  | String s -> n (sprintf "StringVal '%s'" s) []
  | Var id -> n (sprintf "Var '%s'" id) []
  | ContOf e -> n "ContOf" (tExp e)
  | Apply(id, es) -> n (sprintf "Apply '%s'" id) (tExps es)
  | Array es -> n "Array" (tExps es)
  | Prop(exp, propName) -> n (sprintf "Prop '%s'" propName) (tExp exp)
  | ArrayAcc(arrExp, indexExp) -> n "ArrayAccess" (tExps [arrExp; indexExp])

and tExps = List.collect tExp

let rec tStm = function
  | Asg(target, value) -> n "Assign" (tExps [target; value])
  | ArrayAsg(id, index, value) -> n "Assign" (tExps [id; index; value])
  | PrintLn(e) -> n "Print" (tExp e)
  | Seq(stms) -> n "Seq" (tStms stms)
  | While(cond, body) -> n "While" (List.concat [tExp cond; tStm body])
  | Block(decls, body) -> n "Block" (List.concat [tDecls decls; tStm body])
  | Call(applyExp) -> n "Call" (tExp applyExp)
  | Cond(cond,yes,no) -> n "Cond" (List.concat [tExp cond; tStms [yes; no]])
  | Return(e) -> n "Return" (tExp e)

and tStms = List.collect tStm

and tDecl = function
  | VarDec (_, e) ->
    n (sprintf "VarDec %s : %s" "h" "f") (tExp e)
  | ProcDec (retTy, isRec, name, args, body) ->
    let recString = if isRec then "rec " else ""
    n (sprintf "%sProcDec <%s>" recString (tType retTy)) (List.concat [(tTypedIds args); (tStm body)])
  | ArrayDec (ti, len, init) ->
    n "ArrayDec" (List.concat [tTypedId ti; tExp init])
  | Decls(file) ->
    n (sprintf "Decls %s" file) []

and tTypedId (TypedId(t, i)) = n (sprintf "%s:%s" i (tType t)) []

and tTypedIds = List.collect tTypedId

and tType = function
  | VoidT -> "void"
  | IntT -> "int"
  | BoolT -> "bool"
  | StringT -> "string"
  | RefT t -> sprintf "ref<%s>" (tType t)
  | ArrayT t -> sprintf "array<%s>" (tType t)
  | ProcT(t, ps) -> "proc"

and tDecls = List.collect tDecl

and transform = List.head << tStm
