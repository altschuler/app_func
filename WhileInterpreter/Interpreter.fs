(* Interpreter for a simple WHILE-language. *)
(* Based on a natural semantics of WHILE    *)

(* Remember to regenerate the parser and the lexer using the commands 
   in README.txt if you modified the parser and lexer          *)

module Interpreter 

open System
open AST

type state = string -> int
exception State

(* val update: string -> int -> state -> state  *)
let update x v s = fun y -> if x=y then v else s y

(* val A: aExp -> state -> int *)
let rec A a s  = 
    match a with 
    | Int n         -> n
    | Var x         -> s x
    | Add(a1, a2)   -> A a1 s + A a2 s
    | Mul(a1, a2)   -> A a1 s * A a2 s
    | Sub(a1, a2)   -> A a1 s - A a2 s

(* val B: bExp -> state -> bool *)
let rec B b s =
    match b with 
    | TT            -> true
    | FF            -> false
    | Eq(a1, a2)    -> A a1 s = A a2 s
    | Ltn(a1, a2)   -> A a1 s < A a2 s
    | Not b'        -> not (B b' s)
    | And(b1, b2)   -> B b1 s && B b2 s

(* val I: stm -> state -> state *)
let rec I stm s =
    match stm with 
    | Asgn(x, a)        -> update x (A a s) s
    | Skip              -> s
    | Seq(stm1, stm2)   -> I stm2 (I stm1 s)
    | ITE(b,stm1,stm2)  -> if B b s then I stm1 s else I stm2 s
    | While(b, stm)     -> if B b s then I (While(b,stm)) (I stm s) else s