// Michael R. Hansen 03-01-2014
module AST
open System

type Id = string

type Type = | VoidT | IntT | BoolT | StringT
            | RefT of Type | ArrayT of Type | ProcT of Type * List<Type>

and TypedId = TypedId of Type * Id

type Exp = | Int of int
           | Bool of bool
           | String of string
           | Var of Id
           | ContOf of Exp
           | Apply of Id * List<Exp>
           | Array of List<Exp>
           | Prop of Exp * Id // (exp, prop name)
           | ArrayAcc of Exp * Exp // (arr, index)

and  Stm = | Asg of Exp * Exp
           | ArrayAsg of Exp * Exp * Exp // (id, index, value)
           | PrintLn of Exp
           | Seq of List<Stm>
           | While of Exp * Stm
           | Block of List<Dec> * Stm
           | Call of Exp
           | Cond of Exp * Stm * Stm
           | Return of Exp

and Dec  = | VarDec of TypedId * Exp
           | ProcDec of Type * Boolean * Id * List<TypedId> * Stm
           | ArrayDec of TypedId * Exp * Exp // (name, length, init value)
