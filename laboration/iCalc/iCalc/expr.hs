-- A simple calculator based on my previous exercise with EXPR
-- This time without variables and function applications
-- The code that handles those have been turned into comment below
--
-- calc:: String -> Float computes the value of an expression
-- given as a string
--
-- iCalc :: IO () reads expressions from stdin until an empty line
-- is entered, and writes the values of the expressions to stdout
--
-- Håkan Jonsson, 2017

import Data.Char

data EXPR = Const Int
--   | Var String
     | Op String EXPR EXPR
--   | App String EXPR
       deriving (Eq, Ord, Show)

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
    
--  buildvar :: String -> (EXPR,String)
--  buildvar xs = until (notfirst isLetter) accletters (Var "", xs)
--    where
--      accletters :: (EXPR,String) -> (EXPR,String)
--      accletters (Var s, y:ys) = (Var (s ++[y]), ys)
    
    
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
    buildfactor ('(':xs) =
      case buildexpr xs of (e, ')':ws) -> (e, ws); _ -> error "missing factor"
    buildfactor (x:xs)
      | isDigit x = buildnumber (x:xs)
--    | isLetter x = case buildvar (x:xs) of
--                     (Var s, '(':zs) -> let (e,ws)=buildfactor ('(':zs) in (App s e,ws)
--                     p -> p
      | otherwise = error "illegal symbol"

unparse :: EXPR -> String
unparse (Const n) = show n
-- unparse (Var s) = s
unparse (Op oper e1 e2) = "(" ++ unparse e1 ++ oper ++ unparse e2 ++ ")"

eval :: EXPR -> [(String,Float)] -> Float
eval (Const n) _ = fromIntegral n
-- eval (Var x) env = case lookup x env of Just y -> y ; _ -> error (x ++ " undefined")
eval (Op "+" left right) env = eval left env + eval right env
eval (Op "-" left right) env = eval left env - eval right env
eval (Op "*" left right) env = eval left env * eval right env
eval (Op "/" left right) env = eval left env / eval right env

-- diff :: EXPR -> EXPR -> EXPR
-- diff _ (Const _) = Const 0
-- diff (Var id) (Var id2)
--   | id == id2 = Const 1
--   | otherwise = Const 0
-- diff v (Op "+" e1 e2) = Op "+" (diff v e1) (diff v e2)
-- diff v (Op "-" e1 e2) = Op "-" (diff v e1) (diff v e2)
-- diff v (Op "*" e1 e2) =
--   Op "+" (Op "*" (diff v e1) e2) (Op "*" e1 (diff v e2))
-- diff v (Op "/" e1 e2) =
--   Op "/" (Op "-" (Op "*" (diff v e1) e1) (Op "*" e1 (diff v e2))) (Op "*" e2 e2)
-- diff _ _ = error "can not compute the derivative"

-- simplify :: EXPR -> EXPR
-- simplify (Const n) = Const n
-- simplify (Var id) = Var id
-- simplify (Op oper left right) =
--   let (lefts,rights) = (simplify left, simplify right) in
--     case (oper, lefts, rights) of
--       ("+",e,Const 0) -> e
--       ("+",Const 0,e) -> e
--       ("*",e,Const 0) -> Const 0
--       ("*",Const 0,e) -> Const 0
--       ("*",e,Const 1) -> e
--       ("*",Const 1,e) -> e
--       ("-",e,Const 0) -> e
--       ("/",e,Const 1) -> e
--       ("-",le,re)     -> if left==right then Const 0 else Op "-" le re
--       (op,le,re)      -> Op op le re

calc = (flip eval []) . parse

iCalc :: IO ()
iCalc =
  do putStr ">"
     line <- getLine
     if (line == [])
       then return ()
       else (do putStr (show (calc line) ++ "\n")
                iCalc)  
             