--import Atype(MyEq, (==))
import Data.Char (ord,chr)
import Data.List
import Data.String
import System.IO
--import Shapes(Shapes, (==))
--import Atype



--- Chapter 10 -------
--- 10.3 -------

f :: Int -> Int
f x = x + 1

f2 :: Int -> Int
f2 x = x + 1

f3 :: Int -> Int
f3 x = x + 1

composeList :: [a -> a] -> a -> a
composeList xs s =  foldr (.) id xs s 

------ 10.7 ------
--flip2 :: (a->b->c) -> (b -> a -> c)
--flip2 f = g
--	where g x y = g y x  

---- 10.8 ----- 

detect :: Char -> Bool
detect x = (\x -> not (x `elem` " \t\n")) x 

------- 10.13 ------
tjo :: [Int] -> [Int]
tjo x = filter (>0) $ map (+1) x

tjo2 :: [Int] -> [Int]
tjo2 x = map (+1) $ filter (>= 0) x
------- 10.14 -----

type Picture = [[Char]]

 

whiteSquare :: [Char]
whiteSquare = "  " 

blackSquare :: [Char]
blackSquare = "##" 

twoWB :: [Char]
twoWB = whiteSquare ++ blackSquare

twoBW :: [Char]
twoBW =  blackSquare ++ whiteSquare

rowWB :: Int -> [Char] 
rowWB 0 = "\n"
rowWB x = twoWB ++ (rowWB (x-2) )

rowBW :: Int -> [Char] 
rowBW 0 = "\n"
rowBW x = twoBW ++ (rowBW (x-2) )


chessBoard :: Int -> Picture
chessBoard 0 = []
chessBoard x = [ rowWB 8] ++ [rowBW 8] ++ chessBoard (x - 2)

printThis :: String -> IO ()
printThis x = putStr x

makeBoard :: Picture -> String
makeBoard (x:xs) = x ++ makeBoard xs 

------ 12.2 -------
--- import Atype ---
--numEqual :: MyEq a => a -> a -> Bool
--numEqual x y = x Atype.== y	

-------  12.3 ------- 
--oneLookupSecond :: MyEq b => [(a,b)] -> b -> a

--oneLookupSecond (x:xs) y 
--	| snd (x) Atype.== y = fst (x)
--	| otherwise = oneLookupSecond xs y 


--oneLookupFirst :: MyEq a => [(a,b)] -> a -> b
--oneLookupFirst (x:xs) y 
--	| fst (x) Atype.== y = snd (x)
--	| otherwise = oneLookupFirst xs y 

---------- 12.4 -----------

------- chapter 13 ---------
----
---------------------------

------ Chapter 14 --------
-------- 14.1 -----------
data Temp = Cold | Hot
	deriving (Eq,Ord,Enum,Show,Read)

data Season = Spring | Summer | Autumn | Winter
	deriving (Eq,Ord,Enum,Show,Read) 

data Shape = Circle Float 
	| Rectangle Float Float 
	| Triangle Float Float Float
	deriving (Eq,Ord,Show,Read) 	

weather :: Season -> Temp 
weather x 
	| x == Summer = Hot
	| otherwise = Cold 

-------- 14.4 --------

perimeter :: Shape-> Float
perimeter (Circle x) = pi * (2*x)
perimeter (Rectangle x y) = (2*x) + (2*y)
perimeter (Triangle a b c) = a + b + c

------- 14.5 --------

area :: Shape -> Float 
area (Circle x) = pi * (2*x*x)
area (Rectangle x y) = x*y
area (Triangle a b c) = sqrt(p*(p-a)*(p-b)*(p-b))
	where p = (a + b + c ) / 2

-------- 14.6 ------- 

isRegular :: Shape -> Bool
isRegular (Circle x) = True
isRegular (Rectangle x y) 
	| x == y = True
	| otherwise = False
isRegular (Triangle a b c)
	| a == b && b == c = True
	|otherwise = False

----- 14.8 --------











