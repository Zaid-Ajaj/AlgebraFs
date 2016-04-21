module Parser 


open FParsec
open System

open Expression
open Patterns
open Algebra
open Evaluation
open Printer


let internal ws = spaces
let internal ch c = skipChar c .>> ws
     
let internal pconst           = pfloat .>> ws           |>> (fun x -> Const x)                           
let internal px               = ch 'x'                  |>> (fun n -> X(1.0,1.0))
let internal pxWithCoef       = pfloat .>> ch 'x'       |>> (fun n -> X(n,1.0))
let internal pxWithPower        = pstring "x^" >>. pfloat |>> (fun n -> (X(1.0,n)))

let internal pPi: Parser<Expr,unit> = pstring "pi" .>> ws    
                                        |>> (fun n -> Const(Math.PI))

let internal pE: Parser<Expr,unit>  = pstring "e"  .>> ws
                                        >>= fun a -> preturn (Const(Math.E))

let internal pxWithCoefAndPower = pfloat >>= fun a ->
                                pstring "x^" >>= fun _ ->
                                pfloat >>= fun n ->
                                preturn (X(a,n))
     
let internal pmix = (attempt pxWithCoefAndPower) <|> 
                    (attempt pxWithCoef) <|> 
                    (attempt pxWithPower) <|>
                    pconst <|> px .>> ws <|> pPi <|> pE
            
let internal opp   = new OperatorPrecedenceParser<_,_,_>()
let internal expr  = opp.ExpressionParser
                     
opp.TermParser <- choice [(ch '(') >>. expr .>> (ch ')'); pmix]

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
  ("cosh"  , cosh') ]
    |> Seq.iter (fun (name,func) -> 
        opp.AddOperator(PrefixOperator(name, ws, 5, true, func)))

opp.AddOperator (InfixOperator("^", ws, 4, Associativity.Right, fun x y -> power(x,y)))
opp.AddOperator(PrefixOperator("-", ws, 6, true, neg))

let completeExpression = ws >>. expr .>> eof

let parse input = 
    match run completeExpression input with
    | Success(res,_,_) -> res
    | Failure(errMsg,_,_) -> failwith errMsg