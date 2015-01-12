#r "../lib/FSharp.PowerPack.dll"

#load "AST.fs"
#load "Parser.fs"
#load "Lexer.fs"
#load "ParserUtil.fs"
#load "TreeDesign.fs"
#load "TreeDraw.fs"
//#load "TreeDrawBuilder.fs"
#load "TransformAST.fs"

open AST
open ParserUtil
open TreeDesign
open TreeDraw
open TransformAST

open System.IO
open System.Diagnostics

let ast2ps = drawTree << design << transform
let read file = File.ReadAllText(file)
let file2ps = ast2ps << parseString << read

let generateProgram size =
  let decls = List.reduce (+) <| List.replicate size ";int foo : 1"
  let stms = List.reduce (+) <| List.replicate size ";print \"x\""
  sprintf "let int foo : 1%s in print \"x\"%s end" decls stms

let time size =
  let designed = (design << transform << parseString << generateProgram) size
  let stopwatch = Stopwatch.StartNew()
  ignore (drawTree designed)
  stopwatch.Stop()
  stopwatch.Elapsed.TotalMilliseconds

// run once to load all modules
ignore (time 1)

let analyze = List.zip [1 .. 100] (List.map time [1 .. 100])
printfn "%A" analyze
