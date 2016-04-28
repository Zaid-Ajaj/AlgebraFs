module Patterns

// using partial active patterns
open Expression


let (|Sin|_|) = function
    | Func("sin", [expr]) -> Some expr
    | _ -> None

let (|Cos|_|) = function
    | Func("cos", [expr]) -> Some expr
    | _ -> None

let (|Tan|_|) = function
    | Func("tan", [expr]) -> Some expr
    | _ -> None

let (|Sec|_|) = function
    | Func("sec", [expr]) -> Some expr
    | _ -> None

let (|ArcSin|_|) = function 
    | Func(("asin"|"arcsin"), [expr]) -> Some expr
    | _ -> None 

let (|ArcCos|_|) = function 
    | Func(("acos"|"arccos"), [expr]) -> Some expr
    | _ -> None 

let (|ArcTan|_|) = function
    | Func(("atan"|"arctan"), [expr]) -> Some expr
    | _ -> None

let (|Log|_|) = function 
    | Func("log", [expr]) -> Some expr
    | _ -> None

let (|Exp|_|) = function
    | Func("exp", [expr]) -> Some expr
    | _ -> None

let (|Sinh|_|) = function 
    | Func("sinh", [expr]) -> Some expr
    | _ -> None

let (|Cosh|_|) = function 
    | Func("cosh", [expr]) -> Some expr
    | _ -> None

let (|Sum|_|) = function
    | Func("sum", [e1; e2]) -> Some (e1, e2)
    | _ -> None

let (|Prod|_|) = function
    | Func("prod", [e1; e2]) -> Some (e1, e2)
    | _ -> None 

let (|Power|_|) = function
    | Func("pow", [e1; e2]) -> Some (e1, e2)
    | _ -> None

let (|Monomial|_|) = function
    | Prod (Const coeff, Power(Id name, Const power)) -> Some (coeff, name, power)
    | Power(Id name, Const power) -> Some (1.0, name, power)
    | Id name -> Some (1.0, name, 1.0)
    | _ -> None

let (|Sqrt|_|) = function
    | Power(e, Const -0.5) -> Some e
    | _ -> None

let (|Neg|_|) = function
    | Prod(Const -1.0, e)
    | Prod(e, Const -1.0) -> Some e
    | Monomial(n, id, e) when n < 0.0 -> Some (monomial (Id id) -n e)
    | _ -> None

let (|Vector|_|) = function
    | Func(("vector"|"list"), xs) -> Some xs 
    | _ -> None