module Typing

type Type =
    | TArr of Type * Type
    | TCon of string
    | TTuple of Type list
    | TVar of string

    member this.ftv =
        let rec ftv' =
            function
            | TArr(t1, t2) -> Set.union (ftv' t1) (ftv' t2)
            | TCon _ -> Set.empty
            | TTuple ts -> List.map ftv' ts |> Set.unionMany
            | TVar v -> Set.singleton v

        ftv' this
