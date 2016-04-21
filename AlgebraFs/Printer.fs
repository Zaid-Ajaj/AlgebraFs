module Printer

open Expression
open Patterns
open Algebra

let rec pretty e = 
    match simp e with
    | X(1.0, 1.0) -> sprintf "x"
    | X(1.0,n) when n < 0.0 -> sprintf "1/x^%A" (pretty (Const -n))
    | X(1.0, n) when (round n) = n -> sprintf "x^%A" (int n)
    | X(1.0,-0.5) -> sprintf "1/sqrt(x)"
    | X(1.0,0.5) -> sprintf "sqrt(x)"
    | X(-1.0,n) -> sprintf "-x^%A" (pretty <| Const n)
    | X(a,1.0) when (round a) = a -> sprintf "%Ax" (int a)
    | X(a,1.0) -> sprintf "%Ax" a
    | X(1.0,n) -> sprintf "x^%A" n
    | X(a,n) when n < 0.0 -> sprintf "%A * 1/x^%A" (pretty <| Const a) (pretty <| Const -n)
    | X(a,n) when (round a) = a && (round n) = n -> sprintf "%Ax^%A" (int a) (int n)
    | X(a,n) when (round a) = a -> sprintf "%Ax^%A" (int a) n
    | X(a,n) when (round n) = n -> sprintf "%Ax^%A" a (int n)
    | X(a,n) -> sprintf "%Ax^%A" a n
    | Const n when (round n) = n -> sprintf "%A" (int n)
    | Const n -> sprintf "%A" n
    | Neg e -> sprintf "-(%s)" (pretty e)
    | Prod(f, g) -> sprintf "%s * %s" (pretty f) (pretty g)
    | Sum(f, g) -> sprintf "%s + %s" (pretty f) (pretty g)
    | Power(Const n,f) -> sprintf "%s^%s" (pretty (Const(n))) (pretty f)
    | Sqrt f -> sprintf "sqrt(%s)" (pretty f)
    | Power(f, Const n) when n < 0.0 -> sprintf "1/(%s)" (pretty (power(f, Const -n)))
    | Power(f, g) -> sprintf "(%s)^%s" (pretty f) (pretty g)
    | Sin(e) -> sprintf "sin(%s)" (pretty e)
    | Cos(e) -> sprintf "cos(%s)" (pretty e)
    | Log(e) -> sprintf "ln(%s)" (pretty e)
    | Exp(e) -> sprintf "e^(%s)" (pretty e)
    | Tan(e) -> sprintf "tan(%s)" (pretty e)
    | Sec(e) -> sprintf "sec(%s)" (pretty e)
    | Sinh(e) -> sprintf "sinh(%s)" (pretty e)
    | Cosh(e) -> sprintf "cosh(%s)" (pretty e)
    | ArcTan(e) -> sprintf "arctan(%s)" (pretty e)
    | ArcCos(e) -> sprintf "arccos(%s)" (pretty e)
    | ArcSin(e) -> sprintf "arcsin(%s)" (pretty e)
    | UnaryFunc(name, e) -> sprintf "%s(%s)" name (pretty e)
    | BinaryFunc(name, e, e1) -> sprintf "%s(%s, %s)" name (pretty e) (pretty e1)
    | _ -> sprintf "Expression %A cannot be pretty printed" e