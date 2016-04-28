# AlgebraFs
This is my first attempt at making a CAS, it is very primitive that (for now) only simplifies and differentiates a mathematical expression, usage:
```
"sin(5x^2)"
|> Parser.tryParse 
|> function 
    | Undefined msg -> printf "Parse error: %s" msg
    | Value expr -> 
        Calculus.derivative expr (Id "x")
        |> Algebra.fullSimplify
        |> Printer.pretty
        |> printfn "%s" // returns "cos(5*x^2) * 10x"
```
