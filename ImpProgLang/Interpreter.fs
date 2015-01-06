(* Interpreter for a simple WHILE-language. Michael R. Hansen 03-01-2014 *)
(* Based on a natural semantics of WHILE                                 *)

(* Remember to regenerate the parser and the lexer using the commands
   in README.txt if you modified the parser and lexer                    *)

module Interpreter

open System
open AST

/////////////////////////////////
// Types
/////////////////////////////////

type Location = int
type Value    = | IntVal of int
                | BoolVal of bool
                | StringVal of string
                | Reference of Location
                | Primitive of (List<Value> -> Value)
                | ArrayVal of List<Value>
and Env       = Map<string,Value>

// (name, isRed, args, env, body)
type Closure =  string * Boolean * List<string> * Env * Stm

type Content = SimpVal of Value | Proc of Closure |  ArrayCnt of List<Value>

type Store  = Map<Location,Content>

/////////////////////////////////
// Utilities
/////////////////////////////////

let DO_DEBUG = false

let debug s = if DO_DEBUG then printfn "%s" s

let unionMap (p:Map<'a,'b>) (q:Map<'a,'b>) = Map(Seq.concat [ (Map.toSeq p) ; (Map.toSeq q) ])

let setNth ls idx el = List.mapi (fun i el' -> if i = idx then el' else el) ls

/////////////////////////////////
// Functions
/////////////////////////////////

// nextLoc() generates the next available location
let nextLoc: unit -> int =  let n = ref 0
                            let f x = (n := !n+1; !n)
                            f

// exp: Exp -> Env -> Store -> Value * Store
let rec exp e (env:Env) (store:Store) =
    debug <| sprintf "Expression: %A" e
    // TODO: all the "Val" cases seem redundant
    match e with
    | Var v        -> debug <| sprintf "Var: %A" (Map.find v env)
                      match Map.find v env with
                      | Reference loc as refl -> (refl,store)
                      | IntVal _
                      | BoolVal _
                      | StringVal _ as sv     -> (sv, store)
                      | _                     -> failwith "errorYYY"
    | ContOf er    -> match exp er env store with
                      | (Reference loc,store1) -> match Map.find loc store1 with
                                                  | SimpVal res -> (res,store1)
                                                  | _           -> failwith "error2"
                      | (IntVal _ as e, store)
                      | (BoolVal _ as e, store)
                      | (StringVal _ as e, store) -> (e, store)
                      | _                      -> failwith "error1"

    | Apply(f,es) -> let (vals, store1) = expList es env store
                     debug <| sprintf "Application exp: %A" f
                     match Map.find f env with
                     | Primitive f   -> (f vals, store1)
                     | Reference loc ->
                       let (res, store) = app loc env store es
                       match res with
                       | Some r -> (r, store)
                       | None -> failwith "WHAT" // TODO: WTF
                     | _             -> failwith "type error"

    | Array es    ->
      let (vals, store') = expList es env store
      (ArrayVal vals, store')
    | Int i       -> (IntVal i, store)
    | Bool b      -> (BoolVal b,store)
    | String s    -> (StringVal s,store)

    | Prop(e, propName) ->
      let (loc, store') = evalLoc e env store
      let cnt = findCnt loc store'
      let res =
        match (cnt, propName) with
        | (ArrayCnt vals, "length") -> IntVal <| List.length vals
        | _ -> failwith "undefined prop on expression type"

      (res, store')

    | ArrayAcc(arrExp, indexExp)  ->
      let (index, store') = evalInt indexExp env store
      let (arrVals, store'') = findArray arrExp env store'

      (List.nth arrVals index, store'')

and findCnt loc store =
  match Map.tryFind loc store with
  | Some a -> a
  | _ -> failwith "undefined location"

and findArray e env store =
  let (loc, store') = evalLoc e env store
  let cnt = findCnt loc store'
  match cnt with
  | ArrayCnt vals -> (vals, store')
  | _ -> failwith "location is not an array"

and evalLoc e env store =
  match exp e env store with
  | (Reference loc, store) -> (loc, store)
  | _ -> failwith "expected a reference"

and evalString e env store =
  match exp e env store with
  | (StringVal s, store) -> (s, store)
  | _ -> failwith "expected a string"

and evalInt e env store =
  match exp e env store with
  | (IntVal i, store) -> (i, store)
  | _ -> failwith "expected an int"

and expList es env store =
    match es with
    | []       -> ([],store)
    | e::erest -> let (res1, store1) = exp e env store
                  let (ress, store2) = expList erest env store1
                  (res1::ress, store2)

// app: Location -> Store -> option<Value> * Store
and app loc env store args =
  debug <| sprintf "Apply: %A" loc
  match Map.find loc store with
  | Proc (name,isRec,pArgs,pEnv,pBody) as proc ->
    // lookup and add args to current store
    let f = fun (env', s) (arg, pArg) ->
      let (value, s') = exp arg env' s
      (Map.add pArg value env', s)
    let (pEnv', store') = List.fold f (unionMap env pEnv, store) (List.zip args pArgs)
    let (pEnv'', store'') = (pEnv', store')
      // TODO: do we get the ref from union with calling environment?
      // if not isRec
      // then (store, pEnv')
      // else
      //   let loc' = nextLoc()
      //   let s' = Map.add loc proc store
      //   let e' = Map.add name (Reference loc) pEnv'
      //   (s', e')

    // exec body statement in new env
    let (res, store2) = stm pBody pEnv'' store'
    // return empty value and the new store
    (res, store2)
  | _ -> failwith " is not a function"

// stm: Stm -> Env -> Store -> option<Value> * Store
and stm st (env:Env) (store:Store) : option<Value> * Store =
    debug <| sprintf "Statement: %A" st
    match st with
    | Cond(e, t, f) ->
      let (res, store') = exp e env store
      match res with
        | BoolVal true  -> stm t env store'
        | BoolVal false -> stm f env store'
        | _ -> failwith "invalid condition type" // TODO: nicer error

    // TODO: clean.it.up
    | Call(Apply(n, args)) ->
      // find the function reference
      match Map.find n env with
      // find the actual function
      | Reference loc -> app loc env store args
      | _ -> failwith " is undefined"

    | Call(_) -> failwith " invalid CALL syntax" // this should be unreachable

    | ArrayAsg(idExp, indexExp, value) ->
      let (index, store1) = evalInt indexExp env store

      // ignore the store here, as we get the same below in store2
      let (loc, _) = evalLoc idExp env store1

      let (arr, store2) = findArray idExp env store1

      let (elem, store3) = exp value env store

      let updated = setNth arr index elem

      let store5 = Map.add loc (ArrayCnt updated) (Map.remove loc store3)

      (None, store5)

    | Asg(el,e) ->
      let (res, store1) = exp e env store
      let (resl, store2) = exp el env store1
      match resl with
      | Reference loc -> (None, Map.add loc (SimpVal res) store2)
      | _             -> failwith "type error"

    | PrintLn e ->
      let (str, store') = evalString e env store
      printfn "%s" str
      (None, store')

    | Return e ->
      let (res, store') = exp e env store
      (Some res, store')

    | Seq []        -> (None,store)

    | Seq (Return e as re::_) -> stm re env store // ignore rest if it's a return

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

and addContent cnt name env store =
  let loc = nextLoc()
  let env' = Map.add name (Reference loc) env
  let store' = Map.add loc cnt store
  (env', store')

and dec d env store =
    debug <| sprintf "Declaration: %A" d
    match d with
    | ArrayDec(name, lengthExp, initialExp) ->
      let (length, store') = evalInt lengthExp env store
      let (initial, store'') = exp initialExp env store'
      let arr = ArrayCnt <| List.replicate length initial
      addContent arr name env store''

    | ProcDec(isRec, name, args, body) ->
      let fn = Proc (name, isRec, args, env, body)
      addContent fn name env store

    | VarDec(s,e) ->
      match exp e env store with
      | (IntVal _ as res, store1)
      | (BoolVal _ as res, store1)
      | (StringVal _ as res, store1) -> addContent (SimpVal res) s env store
      | _ -> failwith "error"
