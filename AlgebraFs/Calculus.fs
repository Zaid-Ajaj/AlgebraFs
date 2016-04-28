module Calculus

open Expression
open Patterns

/// differentiates an expression i.e. f(x) -> f'(x)
let rec derivative expr x =
    match expr with
    | Const n -> zero
    | Monomial(1.0, id, 1.0) when Id id = x -> Const 1.0
    | Monomial(coeff, id, power) when Id id <> x -> Const 0.0
    | Monomial(coeff, id, 1.0) -> Const coeff
    | Monomial(coeff, id, power) when Id id = x ->  monomial (Id id) (coeff * power) (power - 1.0)
    | Sum(f, g) -> derivative f x + derivative g x
    | Prod(Const n, f) -> Const n * (derivative f x)
    | Prod(f, g) -> (derivative f x) * g + f * (derivative g x)
    | Power(f, Const n) -> Const n * power(f, Const (n-1.0)) * derivative f x
    | Power(Const n,f) -> power(Const n, f) * log (Const n) * derivative f x
    | Power(g,h) -> expr * derivative (h * log g) x
    | Tan(f) -> power(sec (f),Const 2.0) * derivative f x
    | Sec(f) -> mul [sec (f); tan (f); derivative f x]
    | Sin f -> cos  f *  derivative f x
    | Cos f -> neg(sin f) * derivative f x 
    | ArcTan f -> (derivative f x) / (Const 1.0 + power(f, Const 2.0))
    | ArcSin f -> (derivative f x) / sqrt(Const 1.0 + !!(power(f, Const 2.0)))
    | ArcCos f -> (asin >> neg >> fun expr -> derivative expr x) f
    | Exp f -> exp f * (derivative f x)
    | Log f -> (derivative f x) / f
    | Sinh f -> cosh f * derivative f x
    | Cosh f -> sinh f * derivative f x
    | _ -> expr