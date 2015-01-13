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

let file2ps = drawTree << design << transform << parseString << File.ReadAllText

File.WriteAllText("tree.ps", file2ps "program/Factorial1.while")
