open FParsec

printfn "%A" (run Parser.expression "if (True) 1 else 2")

