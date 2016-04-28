module Algebra

open Expression
open Patterns


let rec internal length expr = 
    match expr with
    | Const _
    | Id _
    | Monomial(_) -> 1
    | Func(_, xs) -> 1 + (Seq.map length xs |> Seq.sum)

let internal notSameLength e1 e2 = 
    length e1 <> length e2

let rec internal simp expr =
    match expr with
    | Monomial (coeff, something, power) when coeff = 0.0 -> Const 0.0
    | Monomial (coeff, something, power) when power = 0.0 -> Const coeff
    | Sum(Const 0.0, e) -> simp e
    | Sum(e ,Const 0.0) -> simp e
    | Sum(Const n,Const m) -> Const (m+n)
    | Sum(Monomial (a, id, n), Monomial(b, id', m)) when id = id' && m = n -> monomial (Id id) (a+b) m
    | Sum(e1, e2) -> simp e1 + simp e2
    | Power(e, Const 1.0) -> simp e
    | Power(Const 1.0, e) -> Const 1.0
    | Power(Monomial(a, id, n), Const m) -> monomial (Id id) (a ** m) (n * m)
    | Power(Const n, Const m) -> Const (n**m)
    | Power(Const 0.0, e) -> Const 0.0
    | Power(Cos(e),Const n) when n < 0.0 -> power(sec (simp e),Const -n)
    | Power(e1,e2)  -> power (simp e1, simp e2)
    | Prod(Const 1.0,e) -> simp e
    | Prod(e, Const 1.0) -> simp e
    | Prod(Const 0.0,e) -> Const 0.0
    | Prod(e,Const 0.0) -> Const 0.0
    | Prod(Const n,Const m) -> Const (m*n)
    | Prod(Monomial(a, id, m), Const n) 
    | Prod(Const n, Monomial(a, id, m)) -> monomial (Id id) (n*a) m
    | Prod(Monomial(a, id, m), Monomial(b, id', n)) when id = id' -> monomial (Id id) (a * b) (m + n)
    | Prod(e,Power(e1,Const -1.0)) 
    | Prod(Power(e1,Const -1.0),e) when e = e1 -> Const 1.0
    | Prod(e,Power(Cos(e1),Const m)) when m < 0.0 -> prod(simp e, power(sec (simp e1),Const -m))
    | Prod(Power(Cos(e),Const n),Power(Sin(e1), Const m)) when n < 0.0 && n = -m  && e = e1-> power (tan (simp e), Const m)
    | Prod(Power(Sin(e),Const n),Power(Cos(e1),Const m)) when m < 0.0 && m = -n && e = e1 -> power (tan (simp e),Const n)
    | Prod(Tan(e), Cos(e')) when e = e' -> sin (simp e)
    | Prod(Sin(e),Sec(e'))
    | Prod(Sec(e),Sin(e')) when e = e' -> tan (simp e)
    | Prod(Power(e1,Const n),Power(e2,Const m)) when e1 = e2 -> power (simp e1, Const(n+m))
    | Prod(e1,e2) when e1 = e2 -> power (simp e1, Const 2.0)
    | Prod(Prod(e1,e2),e) 
    | Prod(e,Prod(e1,e2)) when notSameLength (e1*e) (simp (e1*e)) -> prod(simp (e1*e),simp e2) 
    | Prod(Prod(e1,e2),e) 
    | Prod(e,Prod(e1,e2)) when notSameLength (e2*e) (simp (e2*e)) -> prod(simp (e2*e),simp e1)
    | Prod(e1,e2) -> prod(simp e1, simp e2)
    | Sec(e) -> sec (simp e)
    | Tan(e) -> tan (simp e)
    | Cos e -> cos (simp e)
    | Sin e -> sin (simp e)
    | Exp(Log(e)) -> simp e
    | Exp e -> exp (simp e)
    | Log (Power(e1,e2)) -> (simp e2) * log (simp e1)
    | Log (Prod(e1,e2)) -> log (simp e1) + log (simp e2)
    | Log(Exp(e)) -> simp e
    | Log e -> log (simp e)
    | Cosh e -> cosh (simp e)
    | Sinh e -> sinh (simp e)
    | ArcTan e -> atan (simp e)
    | ArcSin e -> asin (simp e)
    | ArcCos e -> acos (simp e)
    | _ -> expr
    
let rec fullSimplify e = 
    let simplified = simp e
    if length simplified = length e then e else fullSimplify simplified