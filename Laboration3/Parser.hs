module Parser(module CoreParser, T, digit, digitVal, chars, letter, err,
              lit, number, iter, accept, require, token,
              spaces, word, (-#), (#-)) where
import Prelude hiding (return, fail)
import Data.Char
import CoreParser
infixl 7 -#, #- 

type T a = Parser a

err :: String -> Parser a
err message cs = error (message++" near "++cs++"\n")

  
iter :: Parser a -> Parser [a]  
iter m = ((m # (iter m)) >-> cons) ! (return []) 

cons(a, b) = a:b

--Get second value in tuple
(-#) :: Parser a -> Parser b -> Parser b
m -# n = (m # n) >-> snd   

--Get first value in tuple 
(#-) :: Parser a -> Parser b -> Parser a
m #- n = (m # n) >-> fst

--Get first space in string and iterate until no spaces exist. 
spaces :: Parser String
spaces = iter (char ? isSpace) 


--removes spaces in snd string in Parser.  
token :: Parser a -> Parser a
token m = m #- spaces

--Check if first char in string is a letter. 
letter :: Parser Char
letter =  char ? isAlpha

--Gets first word in string
word :: Parser String
word = token (letter # iter letter >-> cons)

--Same pattern as example iterate, do function n times.
chars :: Int -> Parser String
chars 0 = return []
chars n =  char # chars (n-1) >-> cons 

--iterate :: Parser a -> Int -> Parser [a]
--iterate m 0 = return []
--iterate m i = m # iterate m (i-1) >-> cons

--Accepts string if it is a substring of string 
accept :: String -> Parser String
accept w = (token (chars (length w))) ? (==w)

-- Same as accept but calls err with string w if fail.
require :: String -> Parser String
require w  = (token (chars (length w))) ? (==w) ! err w

lit :: Char -> Parser Char
lit c = token char ? (==c)

digit :: Parser Char 
digit = char ? isDigit 

digitVal :: Parser Integer
digitVal = digit >-> digitToInt >-> fromIntegral

number' :: Integer -> Parser Integer
number' n = digitVal #> (\ d -> number' (10*n+d))
          ! return n
number :: Parser Integer
number = token (digitVal #> number')

double :: Parser Char
double = char #> lit