module Program

open System
open ParserUtil
open Interpreter

(* Parse a program in a file *)
let fac = parseFromFile "factorial.while"

(* Define an initial state *)
let s0 = function "x" -> 5 | _ -> raise State

(* Interpret the program *)
let s1 = I fac s0

(* Inspect the resulting state *)
let y1 = s1 "y"
printfn "y1=%i" y1

let x1 = s1 "x"
printfn "x1=%i" x1

Console.ReadKey() |> ignore


