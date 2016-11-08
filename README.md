# AlgebraFs
This project is a simple computer algebra system (CAS), written entirely in pure F#, for pure fun. It uses an extremely simple expression language modelled by the union:
```fsharp
type Expr =
    | Const of float
    | Id of string
    | Func of string * Expr list

```
Constructs in this expression language are either constants that are `float`s, symbolic identifiers of `string` or functions that have a name (the `string` part) and arguments (the `Expr list` part)

# Partial Active patterns
Active patterns are used extensively in this project to faciliate writing functions that operate on these kinds of expressions, example:
```fsharp
let (|Sin|_|) = function
    | Func("sin", [expr]) -> Some expr
    | _ -> None

let (|Cos|_|) = function
    | Func("cos", [expr]) -> Some expr
    | _ -> None

let (|Tan|_|) = function
    | Func("tan", [expr]) -> Some expr
    | _ -> None

```
Having these patterns defined, you could write the following:
```fsharp
// val isTrigonometric : Expr -> bool
let isTrigonometric = function
    | Sin x -> true
    | Cos x -> true
    | Tan x -> true
    | _ -> false
```
So the library is built around these patterns, evaluating an expression and differentiating a single variable (real) functions is built-in, example:
```fsharp
"sin(5x^2)"
|> Parser.tryParse 
|> function 
    | ParseError msg -> printf "Parse error: %s" msg
    | Parsed expr -> 
        Calculus.derivative expr (Id "x")
        |> Algebra.fullSimplify
        |> Printer.pretty
        |> printfn "%s" // returns "cos(5*x^2) * 10x"
```
