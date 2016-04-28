module Parser 


open FParsec
open System

open Expression
open Patterns
open Algebra
open Evaluation
open Printer

type ParseResult = 
    | Parsed of Expr
    | ParseError of string

let ws = spaces
let ch c = skipChar c .>> ws

let identifier : Parser<Expr, unit> =
    let isIdentifierFirstChar c = isLetter c
    let isIdentifierChar c = isLetter c || isDigit c
    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" .>> ws
    |>> function
        | "pi"  -> Const(Math.PI)
        | "e" -> Const(Math.E)
        | id -> Id id
    
let pstr s = pstring s .>> ws 
let parens p = between (pstr "(") (pstr ")") p

let pconst = 
    pfloat >>= fun constant -> 
    ws >>= fun space -> preturn (Const constant)     

let idWithCoeff = 
    pfloat >>= fun coeff ->
    identifier >>= fun id -> preturn (monomial id coeff 1.0)

let idWithCoeffAndPower : Parser<Expr, unit> = 
    pfloat >>= fun coeff -> 
    identifier >>= fun id ->
    pchar '^' >>= fun c ->
    pfloat >>= fun power ->
    preturn <| monomial id coeff power

let functionName : Parser<string, unit> =
    let isIdentifierFirstChar c = isLetter c
    let isIdentifierChar c = isLetter c || isDigit c
    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "function" .>> ws


let opp   = new OperatorPrecedenceParser<Expr,unit,unit>()
let expr  = opp.ExpressionParser

let functionArgs = ws >>. sepBy expr (pstr ",") |> parens .>> ws

let pfunction : Parser<Expr, unit> = 
    functionName >>= fun fname ->
    functionArgs >>= fun args ->
    let returnedExpression =
        match fname, args with
        | "simplify", [expr] -> Algebra.fullSimplify expr
        | "derivative", [expr; Id name] -> Calculus.derivative expr (Id name)
        | "pow", [e1; e2] -> power(e1, e2)
        | "sec", [expr] -> sec expr
        | "csc", [expr] -> Const 1.0 / tan expr
        | name, xs -> Func(name, xs)
    preturn returnedExpression

    
let pmonomial = 
    attempt (pfunction .>> ws) <|>
    attempt (idWithCoeffAndPower .>> ws) <|>
    attempt (idWithCoeff .>> ws) <|>
    identifier .>> ws <|> 
    pconst .>> ws          

                     
opp.TermParser <- choice [(ch '(') >>. expr .>> (ch ')'); pmonomial]

[ ("+", 2, (+))
  ("-", 2, fun (e:Expr) e2 -> e + !!e2)
  ("*", 3, (*))
  ("/", 3, (/)) ]
    |> Seq.iter (fun (sym,prec,func) -> 
        opp.AddOperator (InfixOperator(sym,ws,prec,Associativity.Left,func)))

//[ ("sqrt"  , sqrt)
//  ("sin"   , sin)
//  ("cos"   , cos)
//  ("tan"   , tan)
//  ("sec"   , sec)
//  ("cot"   , fun e -> cos(e) / sin(e))
//  ("csc"   , fun e -> Const 1.0 / sin(e))
//  ("log"   , log)
//  ("ln"    , log)
//  ("exp"   , exp)
//  ("asin"  , asin)
//  ("arcsin", asin)
//  ("acos"  , acos)
//  ("arccos", acos)
//  ("atan"  , atan)
//  ("arctan", atan)
//  ("e^"    , exp)
//  ("sinh"  , sinh)
//  ("cosh"  , cosh)
//  ("simplify", Algebra.fullSimplify)
//  ("print", Printer.print) ]
//    |> Seq.iter (fun (name,func) -> 
//        opp.AddOperator(PrefixOperator(name, ws, 5, true, func)))

opp.AddOperator (InfixOperator("^", ws, 4, Associativity.Right, fun x y -> power(x,y)))
opp.AddOperator(PrefixOperator("-", ws, 6, true, neg))

let completeExpression = ws >>. expr .>> eof

let tryParse (input: string) = 
    match run completeExpression input with
    | Success(result,_,_) -> Parsed result
    | Failure(errMsg,_,_) -> ParseError errMsg