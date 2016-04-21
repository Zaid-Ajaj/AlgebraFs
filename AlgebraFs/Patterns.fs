module Patterns

// using partial active patterns
open Expression

let (|Sin|_|) = function
    | UnaryFunc("sin", expr) -> Some expr
    | _ -> None

let (|Cos|_|) = function
    | UnaryFunc("cos", expr) -> Some expr
    | _ -> None

let (|Tan|_|) = function
    | UnaryFunc("tan", expr) -> Some expr
    | _ -> None

let (|Sec|_|) = function
    | UnaryFunc("sec", e) -> Some e
    | _ -> None

let (|ArcSin|_|) = function 
    | UnaryFunc("arcsin", expr) -> Some expr
    | _ -> None 

let (|ArcCos|_|) = function 
    | UnaryFunc("arccos", expr) -> Some expr
    | _ -> None 

let (|ArcTan|_|) = function
    | UnaryFunc("arctan", expr) -> Some expr
    | _ -> None

let (|Log|_|) = function 
    | UnaryFunc("log", expr) -> Some expr
    | _ -> None

let (|Exp|_|) = function
    | UnaryFunc("exp", expr) -> Some expr
    | _ -> None

let (|Sinh|_|) = function 
    | UnaryFunc("sinh", expr) -> Some expr
    | _ -> None

let (|Cosh|_|) = function 
    | UnaryFunc("cosh", expr) -> Some expr
    | _ -> None

let (|Sum|_|) = function
    | BinaryFunc("sum", e1, e2) -> Some (e1, e2)
    | _ -> None

let (|Prod|_|) = function
    | BinaryFunc("prod", e1, e2) -> Some (e1, e2)
    | _ -> None 

let (|Power|_|) = function
    | BinaryFunc("pow", e1, e2) -> Some (e1, e2)
    | _ -> None

let (|Sqrt|_|) = function
    | Power(e, Const -0.5) -> Some e
    | _ -> None

let (|Neg|_|) = function
    | Prod(Const -1.0, e)
    | Prod(e, Const -1.0) -> Some e
    | _ -> None