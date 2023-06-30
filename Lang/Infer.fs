module Infer

open Typing

type Constraint = Type * Type
type Constraints = Constraint list

type InferState = TypeEnv * Pump

let solve (c: Constraints) (p: Pump) =
    let p = ref p

    let next () =
        let p' = p.Value
        let n, p' = Pump.next p'
        p.Value <- p'
        n

    let rec unify (s: Subst) =
        function
        | [] -> Ok s
        | (xt1, xt2) :: c ->
            match (Type.apply s xt1, Type.apply s xt2) with
            | TVar v1, TVar v2 when v1 = v2 -> unify s c
            | TVar v, t
            | t, TVar v when not (Set.contains v (Type.ftv t)) -> unify (Subst.compose (Subst.singleton v t) s) c
            | TArr(t1, t2), TArr(t3, t4) -> unify s ((t1, t3) :: (t2, t4) :: c)
            | TCon s1, TCon s2 when s1 = s2 -> unify s c
            | TTuple ts1, TTuple ts2 when List.length ts1 = List.length ts2 -> unify s (List.zip ts1 ts2 @ c)
            | TRowEmpty, TRowEmpty -> unify s c
            | TRowExtend(l1, t1, row1), row2 ->
                let rec rewriteRow =
                    function
                    | TRowExtend(l2, t2, row2) ->
                        if (l1 = l2) then
                            Some(t2, row2)
                        else
                            match rewriteRow row2 with
                            | None -> None
                            | Some(t2', row2') -> Some(t2', TRowExtend(l2, t2, row2'))
                    | _ -> None

                let mapSecond f (x, y) = (x, f y)

                let rec replaceInnerType newInnerType =
                    function
                    | TRowExtend(l, t, row2) ->
                        replaceInnerType newInnerType row2
                        |> mapSecond (fun row2' -> TRowExtend(l, t, row2'))
                    | t -> (t, newInnerType)


                match rewriteRow row2 with
                | None ->
                    let newInnerType = next ()
                    let innerType, row2' = replaceInnerType newInnerType row2

                    (TRowExtend(l1, t1, newInnerType), innerType) :: (row1, row2') :: c |> unify s
                | Some(t2', row2') -> (t1, t2') :: (row1, row2') :: c |> unify s

            | _ ->
                sprintf "Unification failed: %s -- %s" (Type.prettyPrint xt1) (Type.prettyPrint xt2)
                |> Error

    unify Subst.empty c, p.Value

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
                let t, p, c1 = infer (env, p) e

                let solution, p = solve c1 p
                let s = solution |> Result.defaultWith (fun msg -> failwith msg)

                let env1 = TypeEnv.apply s env

                let scheme = TypeEnv.generalise (Type.apply s t) env

                let env2 = TypeEnv.extend x scheme env1

                inferDecs (env2, p, c1 @ c) decs

        let env1, p1, c = inferDecs (env, p, []) decs
        let t, p2, c' = infer (env1, p1) e
        t, p2, c @ c'
    | Parser.LetRec(decs, e) ->
        let fix env e p =
            let t1, p, c1 = infer (env, p) e
            let tv, p = Pump.next p
            tv, p, (TArr(tv, tv), t1) :: c1

        let cvs, p = Pump.nextN (List.length decs) p
        let names = List.map fst decs
        let cvsNames = List.zip names cvs

        let env' =
            cvsNames
            |> List.fold (fun env' (name, cv) -> TypeEnv.extend name (Scheme([], cv)) env') env

        let t1, p, c1 =
            fix env' (Parser.Lambda("_bob", Parser.LTuple(List.map (fun (_, e) -> e) decs))) p

        let c2 = (TTuple cvs, t1) :: c1
        let solution, p = solve c2 p
        let s = solution |> Result.defaultWith (fun msg -> failwith msg)

        let env = TypeEnv.apply s env

        let env =
            cvsNames
            |> List.fold
                (fun env' (name, cv) -> TypeEnv.extend name (TypeEnv.generalise (Type.apply s cv) env) env')
                env

        let t, p, c3 = infer (env, p) e
        t, p, c3 @ c2

    | Parser.Op(e1, Parser.Equals, e2) ->
        let t1, p, c1 = infer (env, p) e1
        let t2, p, c2 = infer (env, p) e2
        typeBool, p, (t1, typeInt) :: (t2, typeInt) :: c1 @ c2
    | Parser.Op(e1, _, e2) ->
        let t1, p, c1 = infer (env, p) e1
        let t2, p, c2 = infer (env, p) e2
        typeInt, p, (t1, typeInt) :: (t2, typeInt) :: c1 @ c2
    | Parser.RecordEmpty -> TRowEmpty, p, []
    | Parser.RecordExtend(n, e1, e2) ->
        let t1, p, c1 = infer (env, p) e1
        let t2, p, c2 = infer (env, p) e2
        TRowExtend(n, t1, t2), p, c1 @ c2
    | Parser.RecordSelect(e, n) ->
        let t, p, c = infer (env, p) e
        let t1, p = Pump.next p
        let t2, p = Pump.next p
        t1, p, (TRowExtend(n, t1, t2), t) :: c
    | Parser.Var x ->
        let scheme = TypeEnv.scheme x env

        match scheme with
        | Some s ->
            let t1, p1 = Scheme.instantiate s p
            t1, p1, []
        | _ -> failwithf "Unknown name: %s" x
