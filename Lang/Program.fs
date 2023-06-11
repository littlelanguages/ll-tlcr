open Typing

module Program =
    let runText text =
        let p = Pump.init

        let ast = Parser.parse (text)

        match ast with
        | Result.Ok e ->
            let t, _, cs = Infer.infer (TypeEnv.empty, p) e

            match Infer.solve cs with
            | Result.Ok s ->
                let t = Type.apply s t
                let actual = Interpreter.evaluate e (Interpreter.emptyEnv ())
                printfn "%s" (Interpreter.Value.prettyPrintWith actual t)
                0
            | Result.Error msg ->
                printfn "Type Error: %s" msg
                1

        | Result.Error msg ->
            printfn "Syntax Error: %s" msg
            1


    let readline () =
        printf "> "

        let rec loop acc =
            let line = System.Console.ReadLine()
            let acc = (acc + "\n" + line).Trim()

            if acc.EndsWith(";;") then
                acc
            else
                printf ". "
                loop acc

        loop ""

    [<EntryPoint>]
    let main args =
        if (args.Length = 0) then
            printfn "Welcome to the REPL of the Typed Lambda Calculus with records!"
            printfn "Type '.quit' to exit."
            printfn "Enter a multi-line expression with ;; as a terminator."

            let mutable text = readline ()

            while (text.StartsWith(".quit") |> not) do
                runText text |> ignore
                text <- readline ()

            0
        else
            let filename = args.[0]
            System.IO.File.ReadAllText(filename) |> runText
