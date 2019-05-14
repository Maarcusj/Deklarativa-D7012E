module Statement(T, parse, toString, fromString, exec,makeString) where
import Prelude hiding (return, fail,read,repeat)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr

type T = Statement
data Statement = 
 Skip |
 Begin [Statement]| --Had problem "Begin Statement, just did one statement"
 While Expr.T Statement|
 Repeat Expr.T Statement| -- Check if expr is fullfilled in the end after every run.
 Read String|
 Write Expr.T|
 Assignment String Expr.T|
 If Expr.T Statement Statement 
  deriving Show

--Parser Functions
skip,begin,while,read,write,assignment,iff,repeat :: Parser Statement

--Can't have require first, will result in error use accept.
--Require last if statement has not correct syntax. => Error
skip = (accept "skip" #- require ";") >-> makeSkip      
makeSkip x = Skip

--makeRepeat (x,y) = Repeat y x 
repeat = (accept "repeat" -# parse #- require "until" # Expr.parse #- require ";") >-> makeRepeat
makeRepeat (x,y) = Repeat y x

-- "begin skip; end"
-- Call parse for all statements in begin, how?
-- Solution: Begin [Statement]
begin = ((accept "begin") -# (iter parse) #- (require "end"))>-> makeBegin 
makeBegin x = Begin x

--"while n do n:=n-1;"
-- Part 1 # Part2
while = ((accept "while" -# Expr.parse #- require "do") # parse) >-> makeWhile
makeWhile (x,y) = While (x) (y) 

--"read count;"
read = (accept "read" -# word #- require ";")  >-> makeRead
makeRead x = Read x

-- "write count+1;"
write = (accept "write" -# Expr.parse #- require ";") >-> makeWrite
makeWrite x = Write x  

--"if x then skip; else x:=0-x;"
iff = (accept "if" -# Expr.parse) # (require "then" -# parse #- require "else") # parse>-> makeIf
makeIf ((x,y),z) = If x y z

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e


--Dictionary.insert ("x", 1) dict
--Dictionary.lookup "x" dict
--statement pattern follwed by next states
exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]

exec [] dict input = []

--Follow this pattern
exec (If cond thenStmts elseStmts: stmts) dict input = 
 if (Expr.value cond dict)>0 
 then exec (thenStmts: stmts) dict input
 else exec (elseStmts: stmts) dict input
 
exec (Skip:stmts) dict input = exec (stmts) dict input

exec (Begin statements:stmts) dict input = exec (statements ++ stmts) dict input


--Same as If, but insert while statement after every statement and call exec again. 
exec (While cond statements: stmts) dict input = 
 if (Expr.value cond dict) >0 
 then exec (statements : While cond statements : stmts) dict input -- Place While statement after every statement in while-loop  
 else exec (stmts) dict input

exec (Read str:stmts) dict input = exec stmts (Dictionary.insert (str,head input) dict) (tail input) --Had problem with this. Adds value to dict, make variables accessable.

exec (Write str:stmts) dict input = Expr.value str dict : exec stmts dict input --Had problem with this. Adds value to returned list.

exec (Assignment str exp:stmts) dict input = exec stmts (Dictionary.insert (str,Expr.value exp dict) dict) input

-- Same as while but run until true.
-- Until value is 0 , run statements.   
exec (Repeat cond statements:stmts) dict input =
 if (Expr.value cond dict) >= 0
 then exec (statements : Repeat cond statements : stmts) dict input   
 else exec (stmts) dict input

-- Make string from statements.
-- Adding previous indentation and passing forward to next statement. 
makeString :: T -> String -> String
makeString (Skip) s = s ++ "skip;\n"
makeString (Begin xs) s = s ++ "begin\n" ++ foldl (++) []  (map (\x-> makeString x (" " ++ s)) xs) ++ s ++ "end\n"
makeString (While cond state) s = s ++ "while " ++ toString cond ++ " do\n"++ (makeString state (" " ++ s)) 
makeString (If cond y z) s = s ++ "if " ++ toString cond ++ " then\n"  ++ (makeString y (" " ++ s)) ++ s ++"else\n"  ++ (makeString z (" " ++ s))
makeString (Read str) s = s ++ "read " ++ str ++ ";\n"
makeString (Write exp) s = s ++ "write " ++ toString exp ++ ";\n" 
makeString (Assignment str exp) s = s ++ str ++ ":=" ++ toString exp ++ ";\n"
makeString (Repeat x y ) s = s ++ "repeat\n" ++ (makeString y (s ++ " ")) ++ "until " ++ toString x ++ ";\n"

-- parse from CoreParser , then test if a function works, if not try next.
-- parse => just(statement,String) 
instance Parse Statement where
  parse = skip ! read ! assignment ! begin ! while ! write ! iff ! repeat
  toString = (\x-> makeString x "") 


