#r "../lib/FSharp.PowerPack.dll"

#load "TreeDesign.fs"
#load "TreeDraw.fs"

open TreeDesign
open TreeDraw
open System.IO

let tree =
  Node("A",
    [ Node("B", []);
      Node("C", []);
      Node("D", //[])]);
        [ Node ("D", []);
          Node ("E", []);
          Node ("F",
            [ Node ("G", [])])])])

let designed = design tree

let stream = new StreamWriter("tree.ps", false)
stream.WriteLine(drawTree designed)
stream.Close()
