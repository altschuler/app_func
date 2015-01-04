module ParserUtil

open System.IO
open System.Text
open Microsoft.FSharp.Text.Lexing

open AST
open Lexer
open Parser

let parseString (text:string) =
   let lexbuf = LexBuffer<_>.FromBytes(Encoding.UTF8.GetBytes(text))
   try
       Parser.Main Lexer.tokenize lexbuf
   with e ->
        let pos = lexbuf.EndPos
        printfn "Error near line %d, character %d\n" pos.Line pos.Column
        failwith "parser termination"

let parseFromFile filename =
   if File.Exists(filename) then
       parseString (File.ReadAllLines(filename).[0]) 
   else
       invalidArg "ParserUtil" "File not found"