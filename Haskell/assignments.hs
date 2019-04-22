import Data.Char (ord,chr)
import Data.List
import Data.String
import System.IO

main :: IO ()   
main = return ()

-- test --
square:: Int -> Int
square n = n * n 

double :: Int -> Int
double n = 2*n

exOr :: Bool -> Bool -> Bool
exOr x y = (x || y) && not (x && y)


---- Help -------

iSort :: [Int] -> [Int]
iSort [] = []
iSort (x:xs) = ins x (iSort xs)

ins :: Int -> [Int] -> [Int]
ins x [] = [x]
ins x (y:ys)
	| x <= y = x:(y:ys)
	| otherwise = y:ins x ys	
----------------------------

-- Chapter 3 Assignments
-- 3.7 --
-- Guards --
threeDifferent1 :: Int -> Int -> Int -> Bool
threeDifferent1 x y z | x == y || x == z || y == z = False | otherwise = True


-- konstig att inte bli grön??-- 
-- funkar dock -- 
sre:: Int -> Int
sre n = n * n 

-- Conditions --
threeDifferent2:: Int -> Int-> Int -> Bool
threeDifferent2 x y z = 
	if x == y then False 
	else if x == z then False
	else if y == z then False 
	else True 

-- 3.8 -- 
twoEqual :: Int -> Int -> Bool
twoEqual a b = 
	if a == b then True
	else False

threeEqual :: Int -> Int -> Int -> Bool
threeEqual a b c =
	if twoEqual a b && twoEqual a c then True
	else False

fourEqual :: Int -> Int -> Int -> Int -> Bool
fourEqual a b c d = 
	if threeEqual a b c && twoEqual a d then True
	else False   

-- 3.17 Feeeel --
numRoot :: Float -> Float -> Float -> Float
numRoot a b c = 
	if b**2 - 4*a*c >= 0 then 1 
	else 0

largerRoot :: Float -> Float -> Float -> Float
largerRoot a b c | numRoot a b c == 0 = 0 | otherwise =  negate b + sqrt (b*b - 4*a*c)  

smallerRoot :: Float -> Float -> Float -> Float
smallerRoot a b c | numRoot a b c == 0 = 0 | otherwise =  negate b - sqrt (b*b - 4*a*c)   
--------------------------------------------------------------------------

-------------------------------------------------------------
--
--
-- Chapter 4 -------------------
--
--
-- 4.7 ----------------------
addfunc :: Int -> Int -> Int
addfunc a b = a + b

multfunc :: Int -> Int -> Int
multfunc a b | a == 0 || b == 0 = 0 | otherwise = b + multfunc (a-1) b  

-- 4.8 ----------------------
getRoot :: Int -> Int -> Int
getRoot a b | b^2 > a = b - 1 |otherwise = getRoot a (b + 1)    

intSquareRoot :: Int -> Int
intSquareRoot a = getRoot a 1 

-- 4.9 -------------------- 

--list of values-- 
-- test functions ----
f4:: Int -> Int
f4 n = maximum [0..(n + 1)] 

f3:: Int -> [Int]
f3 n = [0..n] 

f5:: [Int] -> Int
f5 n = maximum n 
---------------------


--- Recursive solution ---
f:: [Int] -> Int
f [] = 0
f n = max (head n) (f(tail n )) 

-- given f function --- 
f1:: Int -> Int ->Int
f1 n b = b + n

f2:: Int -> Int
f2 n = f (map (f1 3) [0..n])
 ---------------------

 -- 4.14 -- 


-- hemma dator----

----------------------

-- Chapter 5-7 -------
-- 5.2 ----  
orderTriple :: (Int,Int,Int) -> (Int,Int,Int)
orderTriple (a,b,c)
	| (a > b && b > c) = (a,b,c)
	| a > b && b < c && a > c= (a,c,b)
	| a < b && a > c = (b,a,c)
	| a < b && a < c && b > c = (b,c,a)
	| a < c && a > b = (c,a,b)
	| a < c && a < b && b < c = (c,b,a)


	
----------------------	
---- 5.10 ------------
--- [ 2*n | n<- [2..6]] , ny lista med värden då n är mellan 2..6

divisors :: Int -> [Int]
divisors 0 = []
divisors a = [ n | n <- [1..10] , a `mod` n == 0 ]  


isPrime :: Int -> Bool
isPrime a 
	| a < 0 = False
isPrime a 
	| length (divisors a) == 2 = True   
	| otherwise = False

---- 5.11 ---------

matches :: Int -> [Int] -> [Int]
matches x [] = [] 
matches x (y:ys) = [x | x `div` y == 1 , x `mod` y == 0] ++ matches x ys 

elem1 :: Int -> [Int] -> Bool
elem1 x [] = False
elem1 x (y:ys) 
	| y == x = True 
	| elem x ys == True = True 
	| otherwise = False        

-------------------
----- 5.22 --------


onSeparateLines :: [String] -> String
onSeparateLines [[]] = ""
onSeparateLines x   
	| length x == 1 = (head x)
	| otherwise = (head x) ++ "\n" ++ onSeparateLines (tail x)

---------------------------
----- 5.23 -----

duplicate :: String -> Int -> String
duplicate [] n = ""
duplicate x n
	|n <= 0 = ""
	| otherwise = x ++ duplicate x (n-1) 
----------------
----- 5.24 ----------
pushRight :: String -> String
pushRight [] = "           "
pushRight x 
	| length x < 12 = pushRight (" " ++ x)
	| otherwise = x 
------------------------

----- 5.24 lab 1----------
pushRight2 :: Int -> String -> String
pushRight2 x y 
	| length y < x = pushRight2 x (" " ++ y)
	| otherwise = y 
------------------------

--------- Chapter 6 -------------
------- 6.29 -------------------






--------------------------------
------
--------- Chapter 7 -----------
------ 7.2 -----------

addTwo :: [Int] -> Int
addTwo [] = 0
addTwo (x:xs) 
	| length (x:xs) >= 2 = x + head xs 
	| otherwise = x  
