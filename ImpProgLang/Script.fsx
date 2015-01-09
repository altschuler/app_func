// Michael R. Hansen 03-01-2014

(* Load the parser and interpreter *)
#r "../lib/FSharp.PowerPack.dll"

//#load "Utils.fs"
#load "AST.fs"
#load "Parser.fs"
#load "Lexer.fs"
#load "ParserUtil.fs"
#load "Interpreter.fs"

open System
open Interpreter
open AST
open ParserUtil

// Variables

let PROGRAM_DIR = "program";;

let filePath f = String.concat "/" [PROGRAM_DIR; f];;

let plusInt = Primitive( function [IntVal i1; IntVal i2] -> IntVal(i1+i2) | _ -> failwith "Invalid arguments" );;
let minusInt = Primitive( function [IntVal i1; IntVal i2] -> IntVal(i1-i2) | _ -> failwith "Invalid arguments" );;
let multInt = Primitive( function [IntVal i1; IntVal i2] -> IntVal(i1*i2) | _ -> failwith "Invalid arguments" );;
let eqInt = Primitive( function [IntVal i1; IntVal i2] -> BoolVal(i1=i2) | _ -> failwith "Invalid arguments" );;
let neqInt = Primitive( function [IntVal i1; IntVal i2] -> BoolVal(i1<>i2) | _ -> failwith "Invalid arguments" );;
let lessEqInt = Primitive( function [IntVal i1; IntVal i2] -> BoolVal(i1<=i2) | _ -> failwith "Invalid arguments" );;
let lessInt = Primitive( function [IntVal i1; IntVal i2] -> BoolVal(i1<i2) | _ -> failwith "Invalid arguments" );;
let gen = let generator = new System.Random()
          generator.Next;;
let randomInt = Primitive( function [IntVal rng] -> IntVal (gen rng) | _ -> failwith "Invalid arguments" );;
let toString = let f vs =  match vs with
                           | [IntVal v] -> StringVal(string v)
                           | [BoolVal v] -> StringVal(string v)
                           | [StringVal v] -> StringVal v
                           | [Reference loc] -> StringVal (sprintf "Ref (loc %A)" loc)
                           | [ArrayVal vals] ->
                             StringVal <| string (List.map (sprintf "%A") vals)
                           | _          -> failwith "error"
               Primitive f;;

let initEnv = Map.ofList [("+",plusInt); ("-",minusInt); ("*",multInt);
                          ("=",eqInt); ("<>",neqInt); ("<=",lessEqInt); ("<",lessInt);
                          ("randomInt", randomInt); ("toString",toString)  ];;

// Parsing from files
// Set current directory
System.IO.Directory.SetCurrentDirectory __SOURCE_DIRECTORY__;;

let runFile file =
  printfn "\n*** Running file: %s ***" file
  let p = parseFromFile <| filePath file
  ignore (stm p initEnv Map.empty);;


let files = [
  "Factorial1.while";
  "Factorial2.while";
  "Factorial3.while";
  "Factorial4.while";
  "Factorial5.while";

  "ArrayProg1.while";
  "ArrayProg2.while";
  "ArrayProg2-v2.while";

  "ErrorMessages.while";
  "ArrayLiteral.while";
  "ClosureScope.while";
  ];;

List.map runFile files;;
