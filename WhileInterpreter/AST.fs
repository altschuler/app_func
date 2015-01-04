module AST
open System

type AExp =                     (* arithmetical expressions *)
    | Int of int                (* numbers                  *)
    | Var of string             (* variables                *)
    | Add of AExp * AExp        (* addition                 *)
    | Mul of AExp * AExp        (* multiplication           *)
    | Sub of AExp * AExp        (* subtraction              *)

type BExp =                     (* boolean expressions      *)
    | TT                        (* true                     *)
    | FF                        (* false                    *)
    | Eq  of AExp * AExp        (* equality                 *)
    | Ltn of AExp * AExp        (* smaller than             *)
    | Not of BExp               (* negation                 *)
    | And of BExp * BExp        (* conjunction              *)

type Stm =                      (* statements             *)
    | Asgn of string * AExp     (* assignment             *)
    | Skip
    | Seq of Stm * Stm          (* sequential composition *)
    | ITE of BExp * Stm * Stm   (* if-then-else           *)
    | While of BExp * Stm       (* while                  *)