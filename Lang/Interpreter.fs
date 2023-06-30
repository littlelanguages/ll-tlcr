module Interpreter

open Parser

type Value =
    | VInt of int
    | VBool of bool
    | VRecord of Map<string, Value>
    | VTuple of Value list
    | VFun of (Value -> Value)

and Env = { mutable env: Map<string, Value> }

let emptyEnv () : Env = { env = Map.empty }

let rec evaluate (e: Expression) (env: Env) : Value =
    match e with
    | LInt i -> VInt i
    | LBool b -> VBool b
    | LTuple es -> VTuple(List.map (fun e -> evaluate e env) es)
    | Var x ->
        match Map.tryFind x env.env with
        | Some v -> v
        | None -> failwithf "Variable %s not found" x
    | Op(e1, op, e2) ->
        let v1 = evaluate e1 env
        let v2 = evaluate e2 env

        match op with
        | Plus ->
            match v1, v2 with
            | VInt i1, VInt i2 -> VInt(i1 + i2)
            | _ -> failwithf "Type error: expected two integers"
        | Minus ->
            match v1, v2 with
            | VInt i1, VInt i2 -> VInt(i1 - i2)
            | _ -> failwithf "Type error: expected two integers"
        | Times ->
            match v1, v2 with
            | VInt i1, VInt i2 -> VInt(i1 * i2)
            | _ -> failwithf "Type error: expected two integers"
        | Divide ->
            match v1, v2 with
            | VInt i1, VInt i2 -> VInt(i1 / i2)
            | _ -> failwithf "Type error: expected two integers"
        | Equals ->
            match v1, v2 with
            | VInt i1, VInt i2 -> VBool(i1 = i2)
            | VBool b1, VBool b2 -> VBool(b1 = b2)
            | _ -> failwithf "Type error: expected two integers or two booleans"
    | If(e1, e2, e3) ->
        let v1 = evaluate e1 env

        match v1 with
        | VBool b -> if b then evaluate e2 env else evaluate e3 env
        | _ -> failwithf "Type error: expected a boolean"
    | Lambda(x, e) ->
        let f = fun v -> evaluate e { env = Map.add x v env.env }
        VFun f
    | App(e1, e2) ->
        let v1 = evaluate e1 env
        let v2 = evaluate e2 env

        match v1 with
        | VFun f -> f v2
        | _ -> failwithf "Type error: expected a function"
    | Let(decls, e)
    | LetRec(decls, e) ->
        List.iter (fun (x, e) -> env.env <- Map.add x (evaluate e env) env.env) decls

        evaluate e env
    | RecordEmpty -> VRecord(Map.empty)
    | RecordExtend(n, e1, e2) ->
        let v1 = evaluate e1 env
        let v2 = evaluate e2 env

        match v2 with
        | VRecord rs -> VRecord(Map.add n v1 rs)
        | _ -> failwithf "Type error: expected a record"
    | RecordSelect(e, n) ->
        let v = evaluate e env

        match v with
        | VRecord rs ->
            match Map.tryFind n rs with
            | Some v -> v
            | None -> failwithf "Field %s not found" n
        | _ -> failwithf "Type error: expected a record"

module Value =
    let rec prettyPrint v =
        match v with
        | VInt i -> sprintf "%d" i
        | VBool b -> sprintf "%b" b
        | VRecord rs ->
            sprintf
                "{%s}"
                (String.concat ", " (List.map (fun (n, v) -> sprintf "%s = %s" n (prettyPrint v)) (Map.toList rs)))
        | VTuple vs -> sprintf "(%s)" (String.concat ", " (List.map prettyPrint vs))
        | VFun _ -> "function"

    let rec prettyPrintWith v t =
        sprintf "%s: %s" (prettyPrint v) (Typing.Type.prettyPrint t)
