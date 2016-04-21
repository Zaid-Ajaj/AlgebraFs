# AlgebraFs
This is my first attempt at making a CAS, it is very primitive that (for now) only simplifies and differentiates a mathematical expression, usage:
```
"cos(5x^2)"
|> Parser.parse
|> Algebra.derivative
|> Algebra.fullSimplify
|> Printer.pretty
|> printfn "%s" // returns "-10x * sin(5x^2)"
```
