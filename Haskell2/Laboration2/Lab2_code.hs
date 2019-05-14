-- Code to Haskell lab assignment 2 in the course D7012E by HÃ¥kan Jonsson

import Data.Char

data EXPR = Const Int
     | Var String
     | Op String EXPR EXPR
     | App String EXPR deriving (Eq, Ord, Show)

parse :: String -> EXPR
parse = fst . buildexpr
  where
    notfirst p (_,[]) = True
    notfirst p (_,x:xs) = not (p x)
    
    buildnumber :: String -> (EXPR,String)
    buildnumber xs = until (notfirst isDigit) accdigits (Const 0, xs)
      where
        accdigits :: (EXPR,String) -> (EXPR,String)
        accdigits (Const n, y:ys) = (Const(10*n+(ord y - 48)), ys)
    
    buildvar :: String -> (EXPR,String)
    buildvar xs = until (notfirst isLetter) accletters (Var "", xs)
      where
        accletters :: (EXPR,String) -> (EXPR,String)
        accletters (Var s, y:ys) = (Var (s ++[y]), ys)
    
    
    buildexpr :: String -> (EXPR,String)
    buildexpr xs = until (notfirst (\c -> c=='-' || c=='+')) accterms (buildterm xs)
      where
        accterms :: (EXPR,String) -> (EXPR,String)
        accterms (term, y:ys) = (Op (y:[]) term term1, zs)
          where
            (term1,zs) = buildterm ys
    
    buildterm :: String -> (EXPR,String)
    buildterm xs = until (notfirst (\c -> c=='*' || c=='/')) accfactors (buildfactor xs)
      where
        accfactors :: (EXPR,String) -> (EXPR,String)  
        accfactors (fact, y:ys) = (Op (y:[]) fact fact1, zs)
          where
            (fact1,zs) = buildfactor ys
    
    buildfactor :: String -> (EXPR,String)
    buildfactor [] = error "missing factor"
    buildfactor ('(':xs) =  case buildexpr xs of (e, ')':ws) -> (e, ws); _ -> error "missing factor"
    buildfactor (x:xs)
      | isDigit x = buildnumber (x:xs)
      | isLetter x = case buildvar (x:xs) of
                       (Var s, '(':zs) -> let (e,ws)=buildfactor ('(':zs) in (App s e,ws)
                       p -> p
      | otherwise = error "illegal symbol"

unparse :: EXPR -> String
unparse (Const n) = show n
unparse (Var s) = s
unparse (Op oper e1 e2) = "(" ++ unparse e1 ++ oper ++ unparse e2 ++ ")"
unparse (App oper e1) = "(" ++ oper ++ unparse e1 ++ ")"


--Added cos,sin,exp,log
-- Follow pattern.
eval :: EXPR -> [(String,Float)] -> Float
eval (Const n) _ = fromIntegral n
eval (Var x) env = case lookup x env of Just y -> y ; _ -> error (x ++ " undefined")
eval (Op "+" left right) env = eval left env + eval right env
eval (Op "-" left right) env = eval left env - eval right env
eval (Op "*" left right) env = eval left env * eval right env
eval (Op "/" left right) env = eval left env / eval right env
eval (App "cos" x) env = cos((eval x env))
eval (App "sin" x) env = sin((eval x env))
eval (App "exp" x) env = exp((eval x env))
eval (App "log" x) env = log((eval x env))  

diff :: EXPR -> EXPR -> EXPR
diff _ (Const _) = Const 0
diff (Var id) (Var id2)
  | id == id2 = Const 1
  | otherwise = Const 0
diff v (Op "+" e1 e2) = Op "+" (diff v e1) (diff v e2)
diff v (Op "-" e1 e2) = Op "-" (diff v e1) (diff v e2)
diff v (Op "*" e1 e2) =
  Op "+" (Op "*" (diff v e1) e2) (Op "*" e1 (diff v e2))
diff v (Op "/" e1 e2) =
  Op "/" (Op "-" (Op "*" (diff v e1) e1) (Op "*" e1 (diff v e2))) (Op "*" e2 e2)
diff v (App "cos" e1) = Op "*" (Op "*" (App "sin" e1) (diff v e1)) (Op "-" (Const 0) (Const 1))
diff v (App "sin" e1) = Op "*" (App "cos" e1) (diff v e1)
diff v (App "exp" e1) = Op "*" (App "exp" e1) (diff v e1)
diff v (App "log" e1) = Op "/" (diff v e1) (e1)   
diff _ _ = error "can not compute the derivative"

simplify :: EXPR -> EXPR
simplify (Const n) = Const n
simplify (Var id) = Var id
simplify (Op oper left right) =
  let (lefts,rights) = (simplify left, simplify right) in
    case (oper, lefts, rights) of
      ("+",e,Const 0) -> e
      ("+",Const 0,e) -> e
      ("*",e,Const 0) -> Const 0
      ("*",Const 0,e) -> Const 0
      ("*",e,Const 1) -> e
      ("*",Const 1,e) -> e
      ("-",e,Const 0) -> e
      ("/",e,Const 1) -> e
      ("-",le,re)     -> if left==right then Const 0 else Op "-" le re
      (op,le,re)      -> Op op le re
simplify (App op x) = App op (simplify x) 

-- pArt 2
-- eval expr [(x,y)]
mkfun::(EXPR,EXPR) -> (Float->Float)           
mkfun (func, Var x) = (\t -> eval func [(x,t)]) -- För varje argument t sätt in skicka in EXPR [(Var x, t)] 

-- Part 3
-- make functions of strings
-- Send to netRaphs to calc new value. 
-- If in range return value, else calculate new value.
findZero::String -> String -> Float -> Float
findZero v body x0 = newtRaph f f' x0
 where
  exp1 = parse body 
  exp2 = parse v
  f = mkfun (exp1, exp2)
  f' = mkfun (diff (exp2) (exp1), (exp2))

--Helper to find zerp.
-- Calc an approximation of root. 
newtRaph::(Float->Float) -> (Float->Float) -> Float -> Float
newtRaph f f' x
 | abs (x - x0) <= 0.0001 = x0
 | otherwise = newtRaph f f' x0 
 where
  x0 = x - ((f x) / (f' x))   


--Test
-- unparse (simplify (diff (Var "x") (parse "exp(sin(2*x))")))
-- mkfun (parse "x*x+2", Var "x")
-- findZero "x" "x*x*x+x-1" 1.0      = 0.68232775
-- findZero "y" "cos(y)*sin(y)" 2.0  =1.5707964
