import Data.Char (ord,chr)
import Data.List

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
-- Chapter 4 Assignments ---
--
--
-- 4.7 --
addfunc :: Int -> Int -> Int
addfunc a b = a + b

multfunc :: Int -> Int -> Int
multfunc a b | a == 0 || b == 0 = 0 | otherwise = b + multfunc (a-1) b  

-- 4.8 --
getRoot :: Int -> Int -> Int
getRoot a b | b^2 > a = b - 1 |otherwise = getRoot a (b + 1)    

intSquareRoot :: Int -> Int
intSquareRoot a = getRoot a 1 

-- 4.9 -- 

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

-----------------------