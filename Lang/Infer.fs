module Infer

open Typing

type Constraint = Type * Type
type Constraints = Constraint list

type InferState = TypeEnv * Pump

let solve (c: Constraints) =
    let rec unify (s: Subst) =
        function
        | [] -> Ok s
        | (t1, t2) :: c ->
            match (Type.apply s t1, Type.apply s t2) with
            | TVar v1, TVar v2 when v1 = v2 -> unify s c
            | TVar v, t
            | t, TVar v when not (Set.contains v (Type.ftv t)) -> unify (Subst.compose (Subst.singleton v t) s) c
            | TArr(t1, t2), TArr(t3, t4) -> unify s ((t1, t3) :: (t2, t4) :: c)
            | TCon s1, TCon s2 when s1 = s2 -> unify s c
            | TRecord m1, TRecord m2 when Map.count m1 = Map.count m2 ->
                unify s ((Map.toSeq m1 |> Seq.map (fun (k, v) -> (v, Map.find k m2)) |> Seq.toList) @ c)
            | TTuple ts1, TTuple ts2 when List.length ts1 = List.length ts2 -> unify s (List.zip ts1 ts2 @ c)
            | t1', t2' ->
                sprintf "Unification failed: %s -- %s" (Type.prettyPrint t1) (Type.prettyPrint t2)
                |> Error

    unify Subst.empty c

let rec infer (env, p) =
    function
    | Parser.App(e1, e2) ->
        let t1, p, c1 = infer (env, p) e1
        let t2, p, c2 = infer (env, p) e2
        let t, p = Pump.next p
        t, p, (t1, TArr(t2, t)) :: c1 @ c2
    | Parser.If(e1, e2, e3) ->
        let t1, p, c1 = infer (env, p) e1
        let t2, p, c2 = infer (env, p) e2
        let t3, p, c3 = infer (env, p) e3
        t2, p, (t1, typeBool) :: (t2, t3) :: c1 @ c2 @ c3
    | Parser.Lambda(x, e) ->
        let t1, p = Pump.next p
        let env' = TypeEnv.extend x (Scheme([], t1)) env
        let t, p, c = infer (env', p) e
        TArr(t1, t), p, c
    | Parser.LBool _ -> typeBool, p, []
    | Parser.LInt _ -> typeInt, p, []
    | Parser.LRecord rs ->
        let ts, p, c =
            List.fold
                (fun (ts, p, c) (x, e) ->
                    let t, p, c' = infer (env, p) e
                    t :: ts, p, c' @ c)
                ([], p, [])
                rs

        TRecord(List.zip (List.map fst rs) (List.rev ts) |> Map.ofList), p, c
    | Parser.LTuple es ->
        let ts, p, c =
            List.fold
                (fun (ts, p, c) e ->
                    let t, p, c' = infer (env, p) e
                    t :: ts, p, c' @ c)
                ([], p, [])
                es

        TTuple(List.rev ts), p, c

    | Parser.Let(decs, e) ->
        let rec inferDecs (env, p, c) =
            function
            | [] -> env, p, c
            | (x, e) :: decs ->
                let t, p1, c1 = infer (env, p) e

                let s = solve c1 |> Result.defaultWith (fun msg -> failwith msg)

                let env1 = TypeEnv.apply s env

                let scheme = TypeEnv.generalise (Type.apply s t) env

                let env2 = TypeEnv.extend x scheme env1

                inferDecs (env2, p1, c1 @ c) decs

        let env1, p1, c = inferDecs (env, p, []) decs
        let t, p2, c' = infer (env1, p1) e
        t, p2, c @ c'
    | Parser.LetRec(decs, e) ->
        let fix env e p =
            let t1, p1, c1 = infer (env, p) e
            let tv, p2 = Pump.next p1
            tv, p2, (TArr(tv, tv), t1) :: c1

        let cvs, p1 = Pump.nextN (List.length decs) p
        let names = List.map fst decs
        let cvsNames = List.zip names cvs

        let env1 =
            cvsNames
            |> List.fold (fun env' (name, cv) -> TypeEnv.extend name (Scheme([], cv)) env') env

        let t1, p2, c1 =
            fix env1 (Parser.Lambda("_bob", Parser.LTuple(List.map (fun (_, e) -> e) decs))) p1

        let c2 = (TTuple cvs, t1) :: c1
        let s = solve c2 |> Result.defaultWith (fun msg -> failwith msg)

        let env2 = TypeEnv.apply s env1

        let env3 =
            cvsNames
            |> List.fold
                (fun env' (name, cv) -> TypeEnv.extend name (TypeEnv.generalise (Type.apply s cv) env2) env')
                env2

        let t, p3, c3 = infer (env3, p2) e
        t, p3, c3 @ c2

    | Parser.Op(e1, Parser.Equals, e2) ->
        let t1, p, c1 = infer (env, p) e1
        let t2, p, c2 = infer (env, p) e2
        typeBool, p, (t1, typeInt) :: (t2, typeInt) :: c1 @ c2
    | Parser.Op(e1, _, e2) ->
        let t1, p, c1 = infer (env, p) e1
        let t2, p, c2 = infer (env, p) e2
        typeInt, p, (t1, typeInt) :: (t2, typeInt) :: c1 @ c2
    | Parser.Var x ->
        let scheme = TypeEnv.scheme x env

        match scheme with
        | Some s ->
            let t1, p1 = Scheme.instantiate s p
            t1, p1, []
        | _ -> failwithf "Unknown name: %s" x
