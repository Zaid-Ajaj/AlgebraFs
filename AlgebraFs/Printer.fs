module Printer

open Expression
open Patterns
open Algebra

let print (e:Expr) = Id (sprintf "%A" e)

let rec pretty e = 
    match e with
    | Monomial (1.0, id, 1.0) -> sprintf "%s" id
    | Monomial (1.0, id, n) when n < 0.0 -> sprintf "1/%s^%f" id (-n)
    | Monomial (1.0, id, n) when (round n) = n -> sprintf "%s^%d" id (int n)
    | Monomial (1.0, id, -0.5) -> sprintf "1/sqrt(%s)" id
    | Monomial (1.0, id, 0.5) -> sprintf "sqrt(%s)" id
    | Monomial (-1.0, id, n) when (round n) = n -> sprintf "-%s^%d" id (int n)
    | Monomial (-1.0, id, n) -> sprintf "-%s^%A" id n
    | Monomial (a, id, 1.0) when (round a) = a -> sprintf "%d%s" (int a) id
    | Monomial (a, id, 1.0) -> sprintf "%A*%s" a id
    | Monomial (1.0, id, n) -> sprintf "%s^%A" id n
    | Monomial (a, id, n) when n < 0.0 -> sprintf "%f * 1/%s^%f" a id -n
    | Monomial (a, id, n) when (round a) = a && (round n) = n -> sprintf "%d*%s^%d" (int a) id (int n)
    | Monomial (a, id, n) when (round a) = a -> sprintf "%A*%s^%A" (int a) id n
    | Monomial (a, id, n) when (round n) = n -> sprintf "%A*%s^%A" a id (int n)
    | Monomial (a, id, n) -> sprintf "%A*%s^%A" a id n
    | Const n when (round n) = n -> sprintf "%A" (int n)
    | Const n -> sprintf "%A" n
    | Neg e -> sprintf "-(%s)" (pretty e)
    | Prod(f, g) -> sprintf "%s * %s" (pretty f) (pretty g)
    | Sum(f, Neg(g)) -> sprintf "%s - %s" (pretty f) (pretty g)
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
    | Func(name, xs) -> 
        xs |> Seq.map pretty
           |> String.concat ", "
           |> sprintf "%s(%s)" name 
    | Vector xs -> 
         xs |> Seq.map pretty
            |> String.concat ", "
            |> sprintf "[%s]"
    | Id name -> name
    | _ -> sprintf "Expression %A cannot be pretty printed" e