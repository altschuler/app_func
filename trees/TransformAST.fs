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
  | _ -> n "Decl" []

and tDecls = List.collect tDecl

and transform = List.head << tStm
