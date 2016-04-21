module Algebra

open Expression
open Patterns

    /// differentiates an expression i.e. f(x) -> f'(x)
let rec derivative expr =
    match expr with
    | Const n -> zero
    | X(a,n) when n = 1.0 -> Const a
    | X(a,n) when n < 1.0 -> Const (a*n) * X(1.0,n - 1.0)
    | X(a,n) -> X(a*n, n - 1.0)
    | Sum(f, g) -> derivative f + derivative g
    | Prod(Const n, f) -> Const n * (derivative f)
    | Prod(f, g) -> (derivative f) * g + f * (derivative g)
    | Power(f, Const n) -> Const n * power(f, Const (n-1.0)) * derivative f
    | Power(Const n,f) -> power(Const n, f) * log'(Const n) * derivative f
    | Power(g,h) -> expr * derivative (h * log' g)
    | Tan(f) -> power(sec'(f),Const 2.0) * derivative f
    | Sec(f) -> mul [sec'(f); tan'(f); derivative f]
    | Sin f -> cos' f *  derivative f
    | Cos f -> neg(sin'(f)) * derivative f
    | ArcTan f -> (derivative f) / (Const 1.0 + power(f, Const 2.0))
    | ArcSin f -> (derivative f) * power(Const 1.0 + !!(power(f,Const 2.0)), Const -0.5)
    | ArcCos f -> (arcsin' >> neg >> derivative) f
    | Exp f -> exp' f * (derivative f)
    | Log f -> derivative f / f
    | Sinh f -> cosh' f * derivative f
    | Cosh f -> sinh' f * derivative f
    | _ -> expr

let rec internal length expr = 
    match expr with
    | Const _
    | X(_) -> 1
    | UnaryFunc(_, e1) -> 1 + length e1
    | Sum(e1,e2) -> 1 + (length e1) + length (e2)
    | Prod(e1,e2) -> 1 + (length e1) + length (e2)
    | Power(e1,e2) -> 1 + (length e1) + length (e2)
    | BinaryFunc(_, e1, e2) -> 1 + (length e1) + length (e2)
    | Cos e | Sin e | Tan e | Sec e
    | ArcTan e | ArcSin e | ArcCos e
    | Log e | Exp e
    | Cosh e | Sinh e  -> 1 + (length e)

let internal notSameLength e1 e2 = 
    length e1 <> length e2

// implemets algebra rules to simplify expression
let rec internal simp expr =
    match expr with
    | X(0.0,_) -> Const 0.0
    | X(a,0.0) -> Const a
    | Sum(Const 0.0, e) -> simp e
    | Sum(e ,Const 0.0) -> simp e
    | Sum(Const n,Const m) -> Const (m+n)
    | Sum(X(a,n),X(b,m)) when m = n -> X(a+b,m)
    | Sum(e1,e2)     -> simp e1 + simp e2
    | Power(e,Const 1.0) -> simp e
    | Power(Const 1.0, e) -> Const 1.0
    | Power(X(a,n),Const m) -> X(a ** m, m * n)
    | Power(Const n, Const m) -> Const (n**m)
    | Power(Const 0.0, e) -> Const 0.0
    | Power(Cos(e),Const n) when n < 0.0 -> power(sec' (simp e),Const -n)
    | Power(e1,e2)  -> power (simp e1,simp e2)
    | Prod(Const 1.0,e) -> simp e
    | Prod(e, Const 1.0) -> simp e
    | Prod(Const 0.0,e) -> Const 0.0
    | Prod(e,Const 0.0) -> Const 0.0
    | Prod(Const n,Const m) -> Const (m*n)
    | Prod(Const n, X(a,m)) when m > 0.0 -> X(n * a,m)
    | Prod(X(a,n),X(b,m)) -> X(a * b,m+n)
    | Prod(X(a,m), Const n) when m > 0.0 -> X(n * a,m)
    | Prod(Sum(e1,e2),e) -> simp (e1*e) + simp (e2*e)
    | Prod(e,Sum(e1,e2)) -> simp (e1*e) + simp (e2*e)
    | Prod(e,Power(e1,Const -1.0)) 
    | Prod(Power(e1,Const -1.0),e) when e = e1 -> Const 1.0
    | Prod(e,Power(Cos(e1),Const m)) when m < 0.0 -> prod(simp e, power(sec' (simp e1),Const -m))
    | Prod(Power(Cos(e),Const n),Power(Sin(e1), Const m)) when n < 0.0 && n = -m  && e = e1-> power (tan' (simp e), Const m)
    | Prod(Power(Sin(e),Const n),Power(Cos(e1),Const m)) when m < 0.0 && m = -n && e = e1 -> power (tan' (simp e),Const n)
    | Prod(Sin(e),Sec(e'))
    | Prod(Sec(e),Sin(e')) when e = e' -> tan' (simp e)
    | Prod(Power(e1,Const n),Power(e2,Const m)) when e1 = e2 -> power (simp e1, Const(n+m))
    | Prod(e1,e2) when e1 = e2 -> power (simp e1, Const 2.0)
    | Prod(Prod(e1,e2),e) 
    | Prod(e,Prod(e1,e2)) when notSameLength (e1*e) (simp (e1*e)) -> prod(simp (e1*e),simp e2) 
    | Prod(Prod(e1,e2),e) 
    | Prod(e,Prod(e1,e2)) when notSameLength (e2*e) (simp (e2*e)) -> prod(simp (e2*e),simp e1)
    | Prod(e1,e2) -> prod(simp e1, simp e2)
    | Sec(e) -> sec'(simp e)
    | Tan(e) -> tan' (simp e)
    | Cos e -> cos' (simp e)
    | Sin e -> sin' (simp e)
    | Exp(Log(e)) -> simp e
    | Exp e -> exp' (simp e)
    | Log (Power(e1,e2)) -> (simp e2) * log' (simp e1)
    | Log (Prod(e1,e2)) -> log' (simp e1) + log'(simp e2)
    | Log(Exp(e)) -> simp e
    | Log e -> log' (simp e)
    | Cosh e -> cosh' (simp e)
    | Sinh e -> sinh' (simp e)
    | ArcTan e -> arctan' (simp e)
    | ArcSin e -> arcsin' (simp e)
    | ArcCos e -> arccos' (simp e)
    | _ -> expr

let rec fullSimplify e = 
    let simplified = simp e
    if simplified <> e && notSameLength simplified e then fullSimplify simplified
    else simplified