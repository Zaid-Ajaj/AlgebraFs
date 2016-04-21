open System

[<EntryPoint>]
let main argv = 
    while true do
        Console.Write("f(x) = ")
        Console.ReadLine()
        |> Parser.parse
        |> Algebra.derivative
        |> Algebra.fullSimp
        |> Printer.pretty
        |> fun result -> Console.WriteLine("f'(x) = {0}", result)
    0 
    