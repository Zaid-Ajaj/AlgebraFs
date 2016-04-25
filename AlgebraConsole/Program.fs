open System
open Parser 
[<EntryPoint>]
let main argv = 
    while true do
        Console.ReadLine()
        |> Parser.tryRead 
        |> function 
            | Undefined msg -> printf "Parse error: %s" msg
            | Value expr -> 
                expr
                |> Printer.pretty
                |> printfn "%s"
    0 
    