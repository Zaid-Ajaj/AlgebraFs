open System
open Parser 

[<EntryPoint>]
let main argv = 
    while true do
        Console.ReadLine()
        |> Parser.tryParse 
        |> function 
            | ParseError msg -> printf "Parse error: %s" msg
            | Parsed expr -> 
                expr
                |> Algebra.fullSimplify
                |> Printer.pretty
                |> printfn "%s"
    0 
    