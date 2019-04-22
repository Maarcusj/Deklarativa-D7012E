import Data.Char

import Prelude hiding (return, iterate)

-- 11

type Parser a = String -> Maybe (a, String)

-- 12

--star  ::  String -> Maybe (Char, String) 
star  ::  Parser Char
star ('*' : xs)  =  Just ('*', xs)
star _  =  Nothing

-- 13

char :: Parser Char
char []= Nothing
char (c:cs) = Just (c, cs)

return :: a -> Parser a
return a cs = Just (a, cs)

fail ::  Parser a 
fail cs = Nothing

-- 14

infix 7 ?

(?) :: Parser a -> (a -> Bool) -> Parser a
(m ? p) cs = 
    case m cs of
    Nothing -> Nothing
    Just(r, s) -> if p r then Just(r, s) else Nothing

star2 = char ? (=='*')

digit = char ? isDigit

-- 15

infixl 3 !

(!) :: Parser a -> Parser a -> Parser a
(m ! n) cs = case m cs of
             Nothing -> n cs 
             mcs -> mcs

starOrDigit = star2 ! digit

lit c = char ? (==c)

star3 = lit '*'

-- 16

infixl 6 #

(#) :: Parser a -> Parser b -> Parser (a, b)
(m # n) cs = 
    case m cs of
    Nothing -> Nothing
    Just(a, cs') -> 
        case n cs' of
        Nothing -> Nothing
        Just(b, cs'') -> Just((a, b), cs'')

twochars :: Parser (Char, Char)
twochars = char # char

funcArrow = 
   twochars ? (\(x,y) -> x=='-' && y=='>')

-- 17

infixl 5 >->

(>->) :: Parser a -> (a -> b) -> Parser b
(m >-> b) cs = 
    case m cs of
    Just(a, cs') -> Just(b a, cs')
    Nothing -> Nothing

digitVal = digit >-> digitToInt

funcArrowStr = funcArrow >-> (\(x,y) -> x:y:[])

thirdchar = twochars # twochars >-> snd >-> fst

-- 18

iterate' :: Parser a -> Int -> Parser [a]
iterate' m 0 = return []
iterate' m i = m # iterate' m (i-1) >-> cons
  where
    cons (a, b) = a:b
  
iterate :: Parser a -> Int -> Parser [a]
iterate m 0 = return []
iterate m i = m # iterate m (i-1) >-> uncurry (:)

nDigits :: Int -> Parser [Int]
nDigits = iterate digitVal

iter :: Parser a -> Parser [a]
iter m = m # iter m >-> uncurry (:) ! return []

numeral' :: Parser [Char]
numeral' = iter digit

numeral :: Parser [Char]
numeral = digit # iter digit >-> uncurry (:)

-- 19

infixl 4 #>

(#>) :: Parser a -> (a -> Parser b) -> Parser b 
(p #> k) cs = 
    case p cs of
    Nothing -> Nothing
    Just(a, cs') -> k a cs'

twoinrow = char #> lit

threeinrow = twoinrow #> lit