module Expression

type Expr =
    | Const of float
    | Id of string
    | Func of string * Expr list

    static member (+) (e1,e2) = Func("sum",  [e1; e2])
    static member (-) (e1,e2) = Func("sum",  [e1;  Func("prod", [Const -1.0; e2])])
    static member (*) (e1,e2) = Func("prod", [e1; e2])
    static member (/) (e1,e2) = Func("prod", [e1; Func("pow", [e2; Const -1.0])])
    static member (^^) (e1,e2) = Func("pow", [e1; e2])
    static member (!!) e = Func("prod", [Const -1.0; e])
    static member Sin e = Func("sin", [e])
    static member Cos e = Func("cos", [e])
    static member Tan e = Func("tan", [e])
    static member Asin e = Func("asin", [e])
    static member Acos e = Func("acos", [e])
    static member Atan e = Func("atan", [e])
    static member Log e = Func("log", [e])
    static member Exp e = Func("exp", [e])
    static member Sinh e = Func("sinh", [e])
    static member Cosh e = Func("cosh", [e])
    static member Sqrt e = e ^^ Const -0.5




let id name = Id name
let monomial (id:Expr) coeff power = Const coeff * (id ^^ Const power)
let zero = Const 0.0
let minusone = Const (-1.0)
let one = Const 1.0

let sum(x, y)  = x + y
let prod(x, y)  = x * y
let power(e1, e2) = e1 ^^ e2
let div(e1, e2) = prod(e1, power(e2, minusone))
let sec x = Func("sec", [x])
let neg x = prod(minusone, x)
let mul xs = Seq.fold (fun x y -> prod(x, y)) one xs
let plus xs = Seq.fold (fun x y -> sum(x, y)) zero xs