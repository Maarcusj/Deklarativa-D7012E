-- A simple calculator based on lab H2 and the type EXPR
-- This time without variables and function applications
-- The code that handles those have been removed below
--
-- calc:: String -> Float computes the value of an arithmetic
-- expression given as a string
--
-- iCalc :: IO () reads expressions from stdin, until an empty line
-- is entered, and writes the values of the expressions to stdout
--
-- /Håkan Jonsson

import Data.Char

data EXPR = Const Int
     | Op String EXPR EXPR
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
      | otherwise = error "illegal symbol"

eval :: EXPR -> [(String,Float)] -> Float
eval (Const n) _ = fromIntegral n
eval (Op "+" left right) env = eval left env + eval right env
eval (Op "-" left right) env = eval left env - eval right env
eval (Op "*" left right) env = eval left env * eval right env
eval (Op "/" left right) env = eval left env / eval right env

calc = (flip eval []) . parse

iCalc :: IO ()
iCalc =
  do putStr ">"
     line <- getLine
     if (line == [])
       then return ()
       else (do putStr (show (calc line) ++ "\n")
                iCalc)  
             
