module Typing

type Type =
    | TArr of Type * Type
    | TCon of string
    | TRowEmpty
    | TRowExtend of string * Type * Type
    | TTuple of Type list
    | TVar of string

type Subst = Subst of Map<string, Type>

type Scheme = Scheme of List<string> * Type

type Pump = Pump of int

type TypeEnv = TypeEnv of Map<string, Scheme>

let typeBool = TCon "Bool"
let typeInt = TCon "Int"

module Type =
    let rec ftv =
        function
        | TArr(t1, t2) -> Set.union (ftv t1) (ftv t2)
        | TCon _ -> Set.empty
        | TRowEmpty -> Set.empty
        | TRowExtend(_, t, row) -> Set.union (ftv t) (ftv row)
        | TTuple ts -> List.map ftv ts |> Set.unionMany
        | TVar v -> Set.singleton v


    let rec apply ((Subst s') as s) =
        function
        | TArr(t1, t2) -> TArr(apply s t1, apply s t2)
        | TCon _ as t -> t
        | TRowEmpty -> TRowEmpty
        | TRowExtend(l, t, row) -> TRowExtend(l, apply s t, apply s row)
        | TTuple ts -> TTuple(List.map (apply s) ts)
        | TVar v -> Map.tryFind v s' |> Option.defaultValue (TVar v)

    let rec prettyPrint =
        function
        | TArr(TArr _ as t1, t2) -> sprintf "(%s) -> %s" (prettyPrint t1) (prettyPrint t2)
        | TArr(t1, t2) -> sprintf "%s -> %s" (prettyPrint t1) (prettyPrint t2)
        | TCon s -> s
        | TRowEmpty -> "{}"
        | TRowExtend _ as row ->
            let rec f =
                function
                | TRowEmpty -> ""
                | TRowExtend(l, t, row) ->
                    let rest =
                        match row with
                        | TRowEmpty -> ""
                        | TRowExtend _ -> sprintf ", %s" (f row)
                        | _ -> sprintf " | %s" (prettyPrint row)

                    sprintf "%s: %s%s" l (prettyPrint t) rest
                | _ as t -> prettyPrint t

            sprintf "{ %s }" (f row)

        | TTuple ts -> sprintf "(%s)" (List.map prettyPrint ts |> String.concat ", ")
        | TVar v -> v

module Subst =
    let empty = Subst Map.empty

    let singleton k v = Map.empty |> Map.add k v |> Subst

    let compose (Subst s1' as s1) (Subst s2') =
        Map.fold (fun s k v -> Map.add k (Type.apply s1 v) s) s1' s2' |> Subst

    let get k (Subst s) = Map.tryFind k s

    let remove ns (Subst s) =
        Map.filter (fun k _ -> not (Set.contains k ns)) s |> Subst

    let rec prettyPrint =
        function
        | Subst s ->
            Map.toSeq s
            |> Seq.map (fun (k, v) -> sprintf "%s -> %s" k (Type.prettyPrint v))
            |> String.concat ", "

    let size (Subst s) = Map.count s

module Pump =
    let init = Pump 0

    let next (Pump i) = sprintf "V%i" i |> TVar, i + 1 |> Pump

    let nextN n p =
        let rec loop i p =
            if i = 0 then
                [], p
            else
                let v, p = next p
                let vs, p = loop (i - 1) p

                v :: vs, p

        loop n p

module Scheme =
    let ftv (Scheme(vs, t)) =
        Set.ofList vs |> Set.difference (Type.ftv t)

    let apply s (Scheme(vs, t)) =
        let vsNames = Set.ofList vs
        let s = Subst.remove vsNames s

        Scheme(vs, Type.apply s t)

    let instantiate (Scheme(vs, t)) p =
        let ffun (s, p) t =
            let i, p = Pump.next p
            Map.add t i s, p

        let s, p = List.fold ffun (Map.empty, p) vs

        Type.apply (Subst s) t, p

    let prettyPrint (Scheme(vs, t)) =
        sprintf "forall %s. %s" (String.concat ", " vs) (Type.prettyPrint t)


module TypeEnv =
    let empty = TypeEnv Map.empty

    let extend x s (TypeEnv te) = Map.add x s te |> TypeEnv

    let ftv (TypeEnv te) =
        Map.toSeq te |> Seq.map (fun (_, s) -> Scheme.ftv s) |> Set.unionMany

    let apply s (TypeEnv te) =
        Map.map (fun _ -> Scheme.apply s) te |> TypeEnv

    let scheme n (TypeEnv te) = Map.tryFind n te

    let generalise t te =
        Scheme(Set.difference (Type.ftv t) (ftv te) |> Set.toList, t)
