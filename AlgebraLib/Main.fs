namespace MathParser

module Parser =  
    open FParsec
    open System

    type Expr =
        | Const of float
        | X of float * float
        | Sum  of Expr * Expr
        | Mul  of Expr * Expr
        | Pow  of Expr * Expr
        | Sin  of Expr
        | Cos  of Expr
        | Tan of Expr
        | Arcsin of Expr
        | Arccos of Expr
        | Arctan of Expr
        | Sec of Expr
        | Exp  of Expr
        | Log  of Expr
        | Sinh of Expr
        | Cosh of Expr

        static member (+) (e1,e2) = Sum(e1,e2)
        static member (-) (e1,e2) = Sum(e1,!e2)
        static member (*) (e1,e2) = Mul(e1,e2)
        static member (/) (e1,e2) = Mul(e1, Pow(e2,Const -1.0))
        static member (^^) (e1,e2) = Pow(e1,e2)
        static member (!!) e = Mul(Const -1.0, e)

    /// differentiates an expression i.e. f(x) -> f'(x)
    let rec D expr =
        match expr with
        | Const n -> Const 0.0
        | X(a,n) when n = 1.0 -> Const a
        | X(a,n) when n < 1.0 -> Const (a*n) * X(1.0,n - 1.0)
        | X(a,n) -> X(a*n, n - 1.0)
        | Sum(f, g) -> D f + D g
        | Mul(Const n, f) -> Const n * (D f)
        | Mul(f, g) -> (D f) * g + f * (D g)
        | Pow(f, Const n) -> Const n * Pow(f,Const(n-1.0)) * D f
        | Pow(Const n,f) -> Pow(Const n,f) * Log(Const n) * D f
        | Pow(g,h) -> expr * D (h * Log(g))
        | Tan(f) -> Pow(Sec(f),Const 2.0) * D f
        | Sec(f) -> Sec(f) * Tan(f) * D f
        | Sin f -> Cos f *  D f
        | Cos f -> !!Sin(f) * D f
        | Arctan f -> (D f) / (Const 1.0 + Pow(f,Const 2.0))
        | Arcsin f -> (D f) * Pow(Const 1.0 + !!(Pow(f,Const 2.0)), Const -0.5)
        | Arccos f -> D (!!Arcsin(f))
        | Exp f -> Exp f * (D f)
        | Log f -> D f / f
        | Sinh f -> Cosh f * D f
        | Cosh f -> Sinh f * D f
        | _ -> expr

    let rec internal length expr = 
        match expr with
        | Const _
        | X(_,_) -> 1
        | Sum(e1,e2)
        | Mul(e1,e2) 
        | Pow(e1,e2) -> 1 + (length e1) + length (e2)
        | Cos e | Sin e | Tan e | Sec e
        | Arctan e | Arcsin e | Arccos e
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
        | Pow(e,Const 1.0) -> simp e
        | Pow(Const 1.0, e) -> Const 1.0
        | Pow(X(a,n),Const m) -> X(a ** m, m * n)
        | Pow(Const n, Const m) -> Const (n**m)
        | Pow(Const 0.0, e) -> Const 0.0
        | Pow(Cos(e),Const n) when n < 0.0 -> Pow(Sec(simp e),Const -n)
        | Pow(e1,e2)  -> Pow(simp e1,simp e2)
        | Mul(Const 1.0,e) -> simp e
        | Mul(e,Const 1.0) -> simp e
        | Mul(Const 0.0,e) -> Const 0.0
        | Mul(e,Const 0.0) -> Const 0.0
        | Mul(Const n,Const m) -> Const (m*n)
        | Mul(Const n, X(a,m)) when m > 0.0 -> X(n * a,m)
        | Mul(X(a,n),X(b,m)) -> X(a * b,m+n)
        | Mul(X(a,m), Const n) when m > 0.0 -> X(n * a,m)
        | Mul(Sum(e1,e2),e) -> simp (e1*e) + simp (e2*e)
        | Mul(e,Sum(e1,e2)) -> simp (e1*e) + simp (e2*e)
        | Mul(e,Pow(e1,Const -1.0)) 
        | Mul(Pow(e1,Const -1.0),e) when e = e1 -> Const 1.0
        | Mul(e,Pow(Cos(e1),Const m)) when m < 0.0 -> Mul(simp e,Pow(Sec(simp e1),Const -m))
        | Mul(Pow(Cos(e),Const n),Pow(Sin(e1), Const m)) when n < 0.0 && n = -m  && e = e1-> Pow(Tan(simp e), Const m)
        | Mul(Pow(Sin(e),Const n),Pow(Cos(e1),Const m)) when m < 0.0 && m = -n && e = e1 -> Pow(Tan(simp e),Const n)
        | Mul(Sin(e),Sec(e'))
        | Mul(Sec(e),Sin(e')) when e = e' -> Tan(simp e)
        | Mul(Pow(e1,Const n),Pow(e2,Const m)) when e1 = e2 -> Pow(simp e1, Const(n+m))
        | Mul(e1,e2) when e1 = e2 -> Pow(simp e1, Const 2.0)
        | Mul(Mul(e1,e2),e) 
        | Mul(e,Mul(e1,e2)) when notSameLength (e1*e) (simp (e1*e)) -> Mul(simp (e1*e),simp e2) 
        | Mul(Mul(e1,e2),e) 
        | Mul(e,Mul(e1,e2)) when notSameLength (e2*e) (simp (e2*e)) -> Mul(simp (e2*e),simp e1)
        | Mul(e1,e2) -> Mul(simp e1, simp e2)
        | Sec(e) -> Sec(simp e)
        | Tan(e) -> Tan(simp e)
        | Cos e -> Cos(simp e)
        | Sin e -> Sin(simp e)
        | Exp(Log(e)) -> simp e
        | Exp e -> Exp(simp e)
        | Log (Pow(e1,e2)) -> (simp e2) * Log(simp e1)
        | Log (Mul(e1,e2)) -> Log(simp e1) + Log(simp e2)
        | Log(Exp(e)) -> simp e
        | Log e -> Log(simp e)
        | Cosh e -> Cosh(simp e)
        | Sinh e -> Sinh(simp e)
        | Arctan e -> Arctan(simp e)
        | Arcsin e -> Arcsin(simp e)
        | Arccos e -> Arccos(simp e)
        | _ -> expr

    let rec fullSimp e = 
        let simplified = simp e
        if simplified <> e && notSameLength simplified e then fullSimp simplified
        else simplified

    /// evaluates an expression 'e' subtituting a variable x with 'value'
    let rec internal eval e value =
        match e with
        | Const n -> n
        | X(a,n) -> a * (value**n)
        | Sum(f,g) -> (eval f value) + (eval g value)
        | Mul(f,g) -> (eval f value) * (eval g value)
        | Pow(f,g) -> (eval f value) ** (eval g value)
        | Cos(f) -> cos (eval f value)
        | Sin(f) -> sin (eval f value)
        | Tan(f) -> tan (eval f value)
        | Sec(f) -> 1.0 / sin(eval f value)
        | Arctan(f) -> atan (eval f value)
        | Arcsin(f) -> asin (eval f value)
        | Arccos(f) -> acos (eval f value)
        | Log(f) -> log (eval f value)
        | Exp(f) -> exp (eval f value)
        | Cosh(f) -> cosh (eval f value)
        | Sinh(f) -> sinh (eval f value)


    /// applies a function f on an initial value i, n times
    let rec internal nest f i n = 
        match n with
        | 0 -> i
        | _ -> nest f (f i) (n-1)

    /// the nth-derivative of function f
    let internal nD f n = nest (D >> fullSimp) f n

    let rec internal factorial (n: bigint) = 
        if n < 2I then 1I else Seq.fold (*) 1I [1I..n]
   

    /// computes a taylor series of function f centered at a of order n
    let internal taylor f a n = 
        let terms = 
            seq { for i in 0..n do
                    let fx = nD f i
                    let value = (1.0/(float <| factorial(bigint i))) * (eval fx a)
                    yield  Const value * Pow(Sum(X(1.0,1.0),!!(Const a)),Const (float i))
        }
        Seq.fold (fun acc term -> Sum(acc,simp term)) (Const 0.0) terms

    let internal ws = spaces
    let internal ch c = skipChar c .>> ws
         
    let internal pconst           = pfloat .>> ws           |>> (fun x -> Const x)                           
    let internal px               = ch 'x'                  |>> (fun n -> X(1.0,1.0))
    let internal pxWithCoef       = pfloat .>> ch 'x'       |>> (fun n -> X(n,1.0))
    let internal pxWithPow        = pstring "x^" >>. pfloat |>> (fun n -> (X(1.0,n)))

    let internal pPi: Parser<Expr,unit> = pstring "pi" .>> ws    
                                            |>> (fun n -> Const(Math.PI))

    let internal pE: Parser<Expr,unit>  = pstring "e"  .>> ws
                                            >>= fun a -> preturn (Const(Math.E))

    let internal pxWithCoefAndPow = pfloat >>= fun a ->
                                               pstring "x^" >>= fun _ ->
                                               pfloat >>= fun n ->
                                               preturn (X(a,n))
         
    let internal pmix = (attempt pxWithCoefAndPow) <|> 
                        (attempt pxWithCoef) <|> 
                        (attempt pxWithPow) <|>
                        pconst <|> px .>> ws <|> pPi <|> pE
                
    let internal opp   = new OperatorPrecedenceParser<_,_,_>()
    let internal expr  = opp.ExpressionParser
                         
    opp.TermParser <- choice[(ch '(') >>. expr .>> (ch ')'); pmix]

    [ ("+", 2, fun (e:Expr) e2 -> e + e2)
      ("-", 2, fun (e:Expr) e2 -> e + !!e2)
      ("*", 3, fun (e:Expr) e2 -> e * e2)
      ("/", 3, fun (e:Expr) e2 -> e / e2) ]
        |> Seq.iter (fun (sym,prec,func) -> 
            opp.AddOperator (InfixOperator(sym,ws,prec,Associativity.Left,func)))

    [ ("sqrt"  , fun e -> Pow(e,Const 0.5))
      ("sin"   , fun e -> Sin e)
      ("cos"   , fun e -> Cos e)
      ("tan"   , fun e -> Tan e)
      ("sec"   , fun e -> Sec e)
      ("cot"   , fun e -> Cos(e) / Sin(e))
      ("csc"   , fun e -> Const 1.0 / Sin(e))
      ("log"   , fun e -> Log e)
      ("ln"    , fun e -> Log e)
      ("exp"   , fun e -> Exp e)
      ("asin"  , fun e -> Arcsin e)
      ("arcsin", fun e -> Arcsin e)
      ("acos"  , fun e -> Arccos e)
      ("arccos", fun e -> Arccos e)
      ("atan"  , fun e -> Arctan e)
      ("arctan", fun e -> Arctan e)
      ("e^"    , fun e -> Exp e)
      ("sinh"  , fun e -> Sinh e)
      ("cosh"  , fun e -> Cosh e)
      ("$"     , fun e -> (D >> fullSimp) e) ]
        |> Seq.iter (fun (name,func) -> 
            opp.AddOperator(PrefixOperator(name, ws, 5, true, func)))

    opp.AddOperator (InfixOperator("^", ws, 4, Associativity.Right, fun x y -> Pow(x,y)))
    opp.AddOperator(PrefixOperator("-", ws, 6,true, fun x -> !!x))

    let completeExpression = ws >>. expr .>> eof


    let rec internal pretty e = 
        match simp e with
        | X(1.0, 1.0) -> sprintf "x"
        | X(1.0,n) when n < 0.0 -> sprintf "1/x^%A" (pretty (Const -n))
        | X(1.0, n) when (round n) = n -> sprintf "x^%A" (int n)
        | X(1.0,-0.5) -> sprintf "1/sqrt(x)"
        | X(1.0,0.5) -> sprintf "sqrt(x)"
        | X(-1.0,n) -> sprintf "-x^%A" (pretty <| Const n)
        | X(a,1.0) when (round a) = a -> sprintf "%Ax" (int a)
        | X(a,1.0) -> sprintf "%Ax" a
        | X(1.0,n) -> sprintf "x^%A" n
        | X(a,n) when n < 0.0 -> sprintf "%A * 1/x^%A" (pretty <| Const a) (pretty <| Const -n)
        | X(a,n) when (round a) = a && (round n) = n -> sprintf "%Ax^%A" (int a) (int n)
        | X(a,n) when (round a) = a -> sprintf "%Ax^%A" (int a) n
        | X(a,n) when (round n) = n -> sprintf "%Ax^%A" a (int n)
        | X(a,n) -> sprintf "%Ax^%A" a n
        | Const n when (round n) = n -> sprintf "%A" (int n)
        | Const n -> sprintf "%A" n
        | Mul(Const -1.0,e) -> sprintf "-(%A)" (pretty e)
        | Mul(f, g) -> sprintf "%A * %A" (pretty f) (pretty g)
        | Sum(f, g) -> sprintf "%A + %A" (pretty f) (pretty g)
        | Pow(Const n,f) -> sprintf "%A^%A" (pretty (Const(n))) (pretty f)
        | Pow(f,Const n) when n = 0.5 -> sprintf "sqrt(%A)" (pretty f)
        | Pow(f,Const n) when n < 0.0 -> sprintf "1/(%A)" (pretty (Pow(f,Const -n)))
        | Pow(f, g) -> sprintf "(%A)^%A" (pretty f) (pretty g)
        | Sin(e) -> sprintf "sin(%A)" (pretty e)
        | Cos(e) -> sprintf "cos(%A)" (pretty e)
        | Log(e) -> sprintf "ln(%A)" (pretty e)
        | Exp(e) -> sprintf "e^(%A)" (pretty e)
        | Tan(e) -> sprintf "tan(%A)" (pretty e)
        | Sec(e) -> sprintf "sec(%A)" (pretty e)
        | Sinh(e) -> sprintf "sinh(%A)" (pretty e)
        | Cosh(e) -> sprintf "cosh(%A)" (pretty e)
        | Arctan(e) -> sprintf "arctan(%A)" (pretty e)
        | Arccos(e) -> sprintf "arccos(%A)" (pretty e)
        | Arcsin(e) -> sprintf "arcsin(%A)" (pretty e)

    let taylorFunc f center orderN = 
        eval (taylor f center orderN)

    /// returns and expression of a taylor series given an expression 'f',
    /// a center 'a' and an order of 'n'
    let taylorExpr f a n = taylor f a n

    let tangentExpr f a = 
        let df = D f
        let slope = eval df a
        let translation = Const ((eval f a) - (eval df a * a))
        Sum(X(slope,1.0), translation)

    let tangentFunc f a = 
        let df = D f
        let slope = eval df a
        let translation = Const ((eval f a) - (eval df a * a))
        eval (Sum(X(slope,1.0), translation))


    let parse input = 
        match run completeExpression input with
        | Success(res,_,_) -> res
        | Failure(errMsg,_,_) -> failwith errMsg

    let prettyPrint e =
        let x = pretty e
        x.Replace(@"""","")

    let parseDiff input = 
        (parse >> D) input

    let parseToFunc input  = 
        (parse >> eval) input
        
    let parseDiffFunc input = 
        (parse >> D >> eval) input

    let parseDiffSimpPrettyPrint input = 
        (parse >> simp >> D >> fullSimp >> simp >> prettyPrint) input;

    let exprToFunc input = 
        eval input

    let evaluateConstantExpr (input:string) = 
        if input.Contains("x") 
            then failwith "expression contains variable"
        else eval (parse input) 0.0