namespace Nim

module Utils =

  let setNth ls idx el = List.mapi (fun i el' -> if i = idx then el else el') ls
