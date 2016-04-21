open System

[<EntryPoint>]
let main argv = 
    while true do
        Console.ReadLine()
        |> Parser.parse
        |> Algebra.derivative
        |> Algebra.fullSimplify
        |> Printer.pretty
        |> printfn "%s"
    0 
    