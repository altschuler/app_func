(* Interpreter for a simple WHILE-language. Michael R. Hansen 03-01-2014 *)
(* Based on a natural semantics of WHILE                                 *)

(* Remember to regenerate the parser and the lexer using the commands
   in README.txt if you modified the parser and lexer                    *)

module Interpreter

open System
open AST

type Location = int
type Value    = | IntVal of int
                | BoolVal of bool
                | StringVal of string
                | Reference of Location
                | Primitive of (List<Value> -> Value)
and Env       = Map<string,Value>


type Closure =  List<string> * Env * Stm

type Content = SimpVal of Value | Proc of Closure |  ArrayCnt of Value [];;

type Store  = Map<Location,Content>

let closureOf(ps,st) env = (ps, env, st)

// nextLoc() generates the next available location
let nextLoc: unit -> int =  let n = ref 0
                            let f x = (n := !n+1; !n)
                            f

// exp: Exp -> Env -> Store -> Value * Store
let rec exp e (env:Env) (store:Store) =
    match e with
    | Var v       -> match Map.find v env with
                     | Reference loc as refl -> (refl,store)
                     | IntVal i              -> printfn "%s" (string i) ; failwith "errorXXX"
                     | _                     -> failwith "errorYYY"
    | ContOf er    -> match exp er env store with
                      | (Reference loc,store1) -> match Map.find loc store1 with
                                                  | SimpVal res -> (res,store1)
                                                  | _           -> failwith "error"
                      | _                   -> failwith "error"

    | Apply(f,es) -> let (vals, store1) = expList es env store
                     match Map.find f env with
                     | Primitive f   -> (f vals, store1)
                     | _              -> failwith "type error"

    | Int i       -> (IntVal i, store)
    | Bool b      -> (BoolVal b,store)
    | String s    -> (StringVal s,store)

and expList es env store =
    match es with
    | []       -> ([],store)
    | e::erest -> let (res1, store1) = exp e env store
                  let (ress, store2) = expList erest env store1
                  (res1::ress, store2)


// stm: Stm -> Env -> Store -> option<Value> * Store
and stm st (env:Env) (store:Store) =
    match st with
    | Call(Apply(n, args)) ->
      // find the function reference
      match Map.find n env with
        | Reference loc as refl ->
          // find the actual function
          match Map.find loc store with
            | Proc (pArgs,pEnv,pBody) ->
              // lookup and add args to current store
              let pEnv' = List.fold (fun env' (Var arg, pArg) ->
                                     let (v, s) = exp (Var arg) env' store
                                     Map.add pArg v env') pEnv (List.zip args pArgs)
              // exec body statement in new env
              let (res, store2) = stm pBody pEnv' store
              // return empty value and the new store
              (None, store2)
            | _ -> failwith " is not a function"
        | _ -> failwith " is undefined"


    | Asg(el,e) -> let (res,store1) = exp e env store
                   let (resl, store2) = exp el env store1
                   match resl with
                   | Reference loc -> (None, Map.add loc (SimpVal res) store2)
                   | _                               -> failwith "type error"


    | PrintLn e -> match exp e env store with
                   | (StringVal s,store1) -> (printfn "%s" s; (None,store1))
                   | _                    -> failwith "error"


    | Seq []        -> (None,store)
    | Seq (st::sts) -> match stm st env store with
                       | (None,store1)   -> stm (Seq sts) env store1
                       | result       -> result

    | While(e,st1)  -> let (res, store1) = exp e env store
                       match res with
                       | BoolVal true  -> match stm st1 env store1 with
                                          | (None,store2) -> stm st env store2
                                          | result     -> result
                       | BoolVal false -> (None, store1)
                       | _             -> failwith "type error"

    | Block(ds,st1) -> let (env1,store1) = decList ds env store
                       stm st1 env1 store1

and decList ds env store =
    match ds with
    | []       -> (env,store)
    | d::drest -> let (env1,store1) = dec d env store
                  decList drest env1 store1

// store ~= memory with locations, env maps
and dec d env store =
    match d with
    | ProcDec(name, args, body) ->
      let fn = Proc (args, env, body)
      let loc = nextLoc()
      let env2 = Map.add name (Reference loc) env
      let store2 = Map.add loc fn store
      (env2, store2)

    | VarDec(s,e) -> let loc = nextLoc()
                     match exp e env store with
                     | (IntVal _ as res, store1)
                     | (BoolVal _ as res, store1)
                     | (StringVal _ as res, store1)
                                                 -> let env2 = Map.add s (Reference loc) env
                                                    let store2 = Map.add loc (SimpVal res) store1
                                                    (env2, store2)
                     | _                         -> failwith "error"
;;
