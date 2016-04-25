module Evaluation

open Expression
open Patterns
open Algebra

/// evaluates an expression 'e' subtituting a variable x with 'value'
let rec internal eval e env  =
    match e with
    | Const n -> n
    | Monomial(a, id, power) -> 
        let valueOfIdentified = Map.find id env
        a * (valueOfIdentified ** power)
    | Sum(f,g) -> (eval f env) + (eval g env)
    | Prod(f,g) -> (eval f env) * (eval g env)
    | Power(f,g) -> (eval f env) ** (eval g env)
    | Cos(f) -> cos (eval f env)
    | Sin(f) -> sin (eval f env)
    | Tan(f) -> tan (eval f env)
    | Sec(f) -> 1.0 / sin(eval f env)
    | ArcTan(f) -> atan (eval f env)
    | ArcSin(f) -> asin (eval f env)
    | ArcCos(f) -> acos (eval f env)
    | Log(f) -> log (eval f env)
    | Exp(f) -> exp (eval f env)
    | Cosh(f) -> cosh (eval f env)
    | Sinh(f) -> sinh (eval f env)


///// applies a function f on an initial value i, n times
//let rec internal nest f i n = 
//    match n with
//    | 0 -> i
//    | _ -> nest f (f i) (n-1)
//
///// the nth-derivative of function f
//let internal nderivative f n = nest derivative f n
//
//let rec internal factorial (n: bigint) = 
//    if n < 2I then 1I else Seq.fold (*) 1I [1I..n]
//
//
///// computes a taylor series of function f centered at a of order n
//let internal taylor f a n = 
//    let terms = 
//        seq { for i in 0..n do
//                let fx = nderivative f i
//                let value = (1.0/(float <| factorial (bigint i))) * (eval fx a)
//                yield  Const value * power (sum (X(1.0,1.0),!!(Const a)),Const (float i))
//    }
//    Seq.fold (fun acc term -> sum (acc,simp term)) (Const 0.0) terms
//
//
//let taylorFunc f center orderN = 
//    eval (taylor f center orderN)
//
///// returns and expression of a taylor series given an expression 'f',
///// a center 'a' and an order of 'n'
//let taylorExpr f a n = taylor f a n
//
//let tangentExpr f a = 
//    let df = derivative f
//    let slope = eval df a
//    let translation = Const ((eval f a) - (eval df a * a))
//    sum (X(slope,1.0), translation)
//
//let tangentFunc f a = 
//    let df = derivative f
//    let slope = eval df a
//    let translation = Const ((eval f a) - (eval df a * a))
//    eval (sum (X(slope,1.0), translation))
//
//
//let evaluateConstantExpr input = eval input 0.0
//
//let exprToFunc input = 
//    eval input