// Michael R. Hansen 03-01-2014
module AST
open System

type Id = string

type Type = IntT | BoolT | StringT | ArrT of Type

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

and Dec  = | VarDec of Id * Exp
           | ProcDec of Boolean * Id * List<Id> * Stm
           | ArrayDec of Id * Exp * Exp // (name, length, init value)
