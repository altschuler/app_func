#r "../lib/FSharp.PowerPack.dll"

#load "AST.fs"
#load "TreeDesign.fs"
#load "TreeDraw.fs"
#load "TransformAST.fs"

open AST
open TreeDesign
open TreeDraw
open TransformAST

open System.IO

let testAst =
  Block ([VarDec (TypedId (IntT,"n"),Int 4); VarDec (TypedId (IntT,"y"),Int 1)],
   Seq
     [While
        (Apply ("<>",[ContOf (Var "n"); Int 0]),
         Seq
           [PrintLn (Apply ("toString",[ContOf (Var "n")]));
            PrintLn (Apply ("toString",[ContOf (Var "y")]));
            Asg (Var "y",Apply ("*",[ContOf (Var "n"); ContOf (Var "y")]));
            Asg (Var "n",Apply ("-",[ContOf (Var "n"); Int 1]))]);
      PrintLn (String "Result is: ");
      PrintLn (Apply ("toString",[ContOf (Var "n")]));
      PrintLn (Apply ("toString",[ContOf (Var "y")]))])

let transformed = transform testAst
let designed = design transformed

let stream = new StreamWriter("tree.ps", false)
stream.WriteLine(drawTree designed)
stream.Close()
