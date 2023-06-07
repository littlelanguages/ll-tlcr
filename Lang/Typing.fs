module Typing

type Type =
    | TArr of Type * Type
    | TCon of string
    | TTuple of Type list
    | TVar of string

type Subst = Subst of Map<string, Type>

let typeBool = TCon "Bool"
let typeInt = TCon "Int"

module Type =
    let rec ftv =
        function
        | TArr(t1, t2) -> Set.union (ftv t1) (ftv t2)
        | TCon _ -> Set.empty
        | TTuple ts -> List.map ftv ts |> Set.unionMany
        | TVar v -> Set.singleton v


    let rec apply ((Subst s') as s) =
        function
        | TArr(t1, t2) -> TArr(apply s t1, apply s t2)
        | TCon _ as t -> t
        | TTuple ts -> TTuple(List.map (apply s) ts)
        | TVar v -> Map.tryFind v s' |> Option.defaultValue (TVar v)

    let rec prettyPrint =
        function
        | TArr(TArr _ as t1, t2) -> sprintf "(%s) -> %s" (prettyPrint t1) (prettyPrint t2)
        | TArr(t1, t2) -> sprintf "%s -> %s" (prettyPrint t1) (prettyPrint t2)
        | TCon s -> s
        | TTuple ts -> sprintf "(%s)" (List.map prettyPrint ts |> String.concat ", ")
        | TVar v -> v

module Subst =
    let empty = Subst Map.empty

    let compose (Subst s1' as s1) (Subst s2') =
        Map.fold (fun s k v -> Map.add k (Type.apply s1 v) s) s1' s2' |> Subst

    let get k (Subst s) = Map.tryFind k s

    let rec prettyPrint =
        function
        | Subst s ->
            Map.toSeq s
            |> Seq.map (fun (k, v) -> sprintf "%s -> %s" k (Type.prettyPrint v))
            |> String.concat ", "

    let size (Subst s) = Map.count s
