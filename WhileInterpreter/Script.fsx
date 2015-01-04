(* Load the parser *)
#r "FSharp.PowerPack.dll"

#load "AST.fs"
#load "Parser.fs"
#load "Lexer.fs"
#load "ParserUtil.fs"
#load "Interpreter.fs"

open System
open Interpreter

(* Parse a program in a file *)
let fac = ParserUtil.parseFromFile (__SOURCE_DIRECTORY__ + "/factorial.while")

(* Define an initial state *)
let s0 = function "x" -> 5 | _ -> raise State

(* Interpret the program *)
let s1 = I fac s0

(* Inspect the resulting state *)
let y1 = s1 "y"
let x1 = s1 "x"
