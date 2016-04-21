module Evaluation

open Expression
open Patterns
open Algebra

/// evaluates an expression 'e' subtituting a variable x with 'value'
let rec internal eval e value =
    match e with
    | Const n -> n
    | X(a,n) -> a * (value**n)
    | Sum(f,g) -> (eval f value) + (eval g value)
    | Prod(f,g) -> (eval f value) * (eval g value)
    | Power(f,g) -> (eval f value) ** (eval g value)
    | Cos(f) -> cos (eval f value)
    | Sin(f) -> sin (eval f value)
    | Tan(f) -> tan (eval f value)
    | Sec(f) -> 1.0 / sin(eval f value)
    | ArcTan(f) -> atan (eval f value)
    | ArcSin(f) -> asin (eval f value)
    | ArcCos(f) -> acos (eval f value)
    | Log(f) -> log (eval f value)
    | Exp(f) -> exp (eval f value)
    | Cosh(f) -> cosh (eval f value)
    | Sinh(f) -> sinh (eval f value)


/// applies a function f on an initial value i, n times
let rec internal nest f i n = 
    match n with
    | 0 -> i
    | _ -> nest f (f i) (n-1)

/// the nth-derivative of function f
let internal nderivative f n = nest derivative f n

let rec internal factorial (n: bigint) = 
    if n < 2I then 1I else Seq.fold (*) 1I [1I..n]


/// computes a taylor series of function f centered at a of order n
let internal taylor f a n = 
    let terms = 
        seq { for i in 0..n do
                let fx = nderivative f i
                let value = (1.0/(float <| factorial (bigint i))) * (eval fx a)
                yield  Const value * power (sum (X(1.0,1.0),!!(Const a)),Const (float i))
    }
    Seq.fold (fun acc term -> sum (acc,simp term)) (Const 0.0) terms


let taylorFunc f center orderN = 
    eval (taylor f center orderN)

/// returns and expression of a taylor series given an expression 'f',
/// a center 'a' and an order of 'n'
let taylorExpr f a n = taylor f a n

let tangentExpr f a = 
    let df = derivative f
    let slope = eval df a
    let translation = Const ((eval f a) - (eval df a * a))
    sum (X(slope,1.0), translation)

let tangentFunc f a = 
    let df = derivative f
    let slope = eval df a
    let translation = Const ((eval f a) - (eval df a * a))
    eval (sum (X(slope,1.0), translation))


let evaluateConstantExpr input = eval input 0.0

let exprToFunc input = 
    eval input