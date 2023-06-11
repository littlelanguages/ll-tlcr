module Parser

open FParsec
open Microsoft.FSharp.Core.Option

type Op =
    | Equals
    | Plus
    | Minus
    | Times
    | Divide

type Expression =
    | App of Expression * Expression
    | If of Expression * Expression * Expression
    | Lambda of string * Expression
    | Let of Declaration list * Expression
    | LetRec of Declaration list * Expression
    | LBool of bool
    | LInt of int
    | LRecord of (string * Expression) list
    | LTuple of Expression list
    | Op of Expression * Op * Expression
    | Var of string

and Declaration = string * Expression

type UserState = unit // doesn't have to be unit, of course

let keyWordSet =
    System.Collections.Generic.HashSet<_>([| "if"; "else"; "in"; "let"; "rec"; "True"; "False" |])

type Parser'<'t> = Parser<'t, UserState>

let ws: Parser'<unit> = CharParsers.spaces

let pint32_ws: Parser'<int32> = pint32 .>> ws

let pchar_ws c : Parser'<char> = pchar c .>> ws

let pstring_ws s : Parser'<string> = pstring s .>> ws

let additiveOps: Parser'<Op> = (pchar_ws '-' >>% Minus) <|> (pchar_ws '+' >>% Plus)

let multiplicativeOps: Parser'<Op> =
    (pchar_ws '*' >>% Times) <|> (pchar_ws '/' >>% Divide)

let factor, factorRef = createParserForwardedToRef<Expression, unit> ()

let multiplicative: Parser'<Expression> =
    factor .>>. many (multiplicativeOps .>>. factor)
    |>> (fun (f, fs) -> List.fold (fun e1 (op, e2) -> Op(e1, op, e2)) f fs)

let additive: Parser'<Expression> =
    multiplicative .>>. many (additiveOps .>>. multiplicative)
    |>> (fun (f, fs) -> List.fold (fun e1 (op, e2) -> Op(e1, op, e2)) f fs)

let relational: Parser'<Expression> =
    additive .>>. opt ((pstring_ws "==" >>% Equals) .>>. additive)
    |>> (fun (e1, e2) ->
        match e2 with
        | Some(op, e2) -> Op(e1, op, e2)
        | None -> e1)

let expression: Parser'<Expression> =
    (many1 (relational .>> ws))
    |>> (fun es -> List.fold (fun e1 e2 -> App(e1, e2)) es.Head es.Tail)

let lowerIdentifier: Parser'<string> =
    let lowerIdentifierString: Parser<string, unit> =
        satisfy isLower .>>. manyChars (satisfy (fun c -> isLetter c || isDigit c))
        .>> ws
        |>> (fun (c, cs) -> (string c) + cs)

    let expectedIdentifier = expected "identifier"

    fun stream ->
        let state = stream.State
        let reply = lowerIdentifierString stream

        if reply.Status <> Ok || not (keyWordSet.Contains(reply.Result)) then
            reply
        else // result is keyword, so backtrack to before the string
            stream.BacktrackTo(state)
            Reply(Error, expectedIdentifier)

let declaration: Parser<Declaration, unit> =
    (lowerIdentifier .>>. many lowerIdentifier .>> pchar_ws '=' .>>. expression
     |>> fun ((name, names), expr) -> (name, List.foldBack (fun n e -> Lambda(n, e)) names expr))

let declarations: Parser<Declaration list, unit> = sepBy declaration (pchar_ws ';')

factorRef.Value <-
    (pchar_ws '(' >>. expression .>> pchar_ws ')')
    <|> (pint32_ws |>> LInt)
    <|> (pstring_ws "True" >>% LBool true)
    <|> (pstring_ws "False" >>% LBool false)
    <|> ((tuple4 (pchar_ws '\\') lowerIdentifier (pstring_ws "->") expression)
         |>> (fun (_, l, _, e) -> Lambda(l, e)))
    <|> (pchar_ws '{'
         >>. sepBy (tuple3 lowerIdentifier (pchar_ws '=') expression) (pchar_ws ';')
         .>> pchar_ws '}'
         |>> (fun fields -> LRecord(List.map (fun (n, _, e) -> (n, e)) fields)))
    <|> (pstring_ws "let" >>. opt (pstring_ws "rec") .>>. declarations
         .>> pstring_ws "in"
         .>>. expression
         |>> (fun ((r, ds), e) -> if (isSome r) then LetRec(ds, e) else Let(ds, e)))
    <|> (pstring_ws "if" >>. pchar_ws '(' >>. expression .>> pchar_ws ')' .>>. expression
         .>> pstring_ws "else"
         .>>. expression
         |>> (fun ((c, t), e) -> If(c, t, e)))
    <|> (lowerIdentifier |>> Var)

let parseProduction p text =
    match run p text with
    | Success(result, _, l) -> Core.Ok result
    | Failure(msg, _, _) -> Core.Error msg

let parse text = parseProduction expression text
