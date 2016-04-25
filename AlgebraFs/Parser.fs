module Parser 


open FParsec
open System

open Expression
open Patterns
open Algebra
open Evaluation
open Printer

type Result<'a> = 
    | Value of 'a
    | Undefined of reason:string

type 'a result = Result<'a>

let internal ws = spaces
let internal ch c = skipChar c .>> ws

let identifier : Parser<Expr, unit> =
    let isIdentifierFirstChar c = isLetter c
    let isIdentifierChar c = isLetter c || isDigit c
    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" .>> ws
    |>> function
        | "pi"  -> Const(Math.PI)
        | "e" -> Const(Math.E)
        | id -> Id id
     
let internal pconst = 
    pfloat >>= fun constant -> 
    ws >>= fun space -> preturn (Const constant)     

let internal idWithCoeff = 
    pfloat >>= fun coeff ->
    identifier >>= fun id -> preturn (monomial id coeff 1.0)

let internal idWithCoeffAndPower : Parser<Expr, unit> = 
    pfloat >>= fun coeff -> 
    identifier >>= fun id ->
    pchar '^' >>= fun c ->
    pfloat >>= fun power ->
    preturn <| monomial id coeff power

let internal pmonomial = 
    attempt (idWithCoeffAndPower .>> ws) <|>
    attempt (idWithCoeff .>> ws) <|>
    identifier .>> ws <|> 
    pconst .>> ws
            
let internal opp   = new OperatorPrecedenceParser<_,_,_>()
let internal expr  = opp.ExpressionParser
                     
opp.TermParser <- choice [(ch '(') >>. expr .>> (ch ')'); pmonomial]

[ ("+", 2, (+))
  ("-", 2, fun (e:Expr) e2 -> e + !!e2)
  ("*", 3, (*))
  ("/", 3, (/)) ]
    |> Seq.iter (fun (sym,prec,func) -> 
        opp.AddOperator (InfixOperator(sym,ws,prec,Associativity.Left,func)))

[ ("sqrt"  , sqrt')
  ("sin"   , sin')
  ("cos"   , cos')
  ("tan"   , tan' )
  ("sec"   , sec')
  ("cot"   , fun e -> cos'(e) / sin'(e))
  ("csc"   , fun e -> Const 1.0 / sin'(e))
  ("log"   , log')
  ("ln"    , log')
  ("exp"   , exp')
  ("asin"  , arcsin')
  ("arcsin", arcsin')
  ("acos"  , arccos')
  ("arccos", arccos')
  ("atan"  , arctan')
  ("arctan", arctan')
  ("e^"    , exp')
  ("sinh"  , sinh')
  ("cosh"  , cosh')
  ("simplify", Algebra.fullSimplify)
  ("derivative", Algebra.derivative)
  ("print", Printer.print) ]
    |> Seq.iter (fun (name,func) -> 
        opp.AddOperator(PrefixOperator(name, ws, 5, true, func)))

opp.AddOperator (InfixOperator("^", ws, 4, Associativity.Right, fun x y -> power(x,y)))
opp.AddOperator(PrefixOperator("-", ws, 6, true, neg))

let completeExpression = ws >>. expr .>> eof

let tryParse (input: string) : Expr result = 
    match run completeExpression input with
    | Success(res,_,_) -> Value res
    | Failure(errMsg,_,_) -> Undefined errMsg