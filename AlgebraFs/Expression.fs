module Expression

type Expr =
    | Const of float
    | X of float * float
    | UnaryFunc of string * Expr
    | BinaryFunc of string * Expr * Expr

    static member (+) (e1,e2) = BinaryFunc("sum", e1,e2)
    static member (-) (e1,e2) = BinaryFunc("sum", e1, BinaryFunc("prod", Const -1.0, e2))
    static member (*) (e1,e2) = BinaryFunc("prod", e1, e2)
    static member (/) (e1,e2) = BinaryFunc("prod", e1, BinaryFunc("pow", e2,Const -1.0))
    static member (^^) (e1,e2) = BinaryFunc("pow", e1, e2)
    static member (!!) e = BinaryFunc("prod", Const -1.0, e)


let zero = Const 0.0
let minusone = Const (-1.0)
let one = Const 1.0
let sum(x, y)  = BinaryFunc("sum", x, y)
let prod(x, y)  = BinaryFunc("prod", x, y)
let power(e1, e2) = BinaryFunc("pow", e1, e2)
let div(e1, e2) = prod(e1, power(e2, minusone))
let sin' x = UnaryFunc("sin", x)
let cos' x = UnaryFunc("cos", x)
let tan' x = UnaryFunc("tan", x)
let arcsin' x = UnaryFunc("arcsin", x)
let arccos' x = UnaryFunc("arccos", x)
let arctan' x = UnaryFunc("arctan", x)
let sec' x = UnaryFunc("sec", x)
let log' x = UnaryFunc("log", x)
let sinh' x = UnaryFunc("sinh", x)
let cosh' x = UnaryFunc("cosh", x)
let sqrt' x = power(x, Const -0.5)
let exp' x = UnaryFunc("exp", x)
let neg x = prod(minusone, x)
let mul xs = Seq.fold (fun x y -> prod(x, y)) one xs
let plus xs = Seq.fold (fun x y -> sum(x, y)) zero xs