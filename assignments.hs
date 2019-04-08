import Data.Char (ord,chr)
import Data.List
import Data.String
import System.IO
import Data.Hashable

main :: IO ()
main = return ()


-- Test ---------------
double :: Int -> Int
double n = n * 2

add :: Int -> Int -> Int
add x y = x + y

sub :: Int -> Int -> Int
sub x y = x - y

mult :: Int -> Int -> Int
mult x y = x * y 

sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = add x (sumList xs)

----- Exercises Chapter 3 -------

---3.7-----
threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent x y z 
 | x /= y && x /= z && y /= z = True 
 | otherwise = False

--3.8 ----

twoEqual :: Int->Int->Bool
twoEqual a b  
 | a /= b = False
 | otherwise = True

threeEqual :: Int->Int->Int->Bool
threeEqual a b c
 | (twoEqual a b) && (twoEqual b c )= True
 | otherwise = False

fourEqual :: Int->Int->Int->Int->Bool
fourEqual a b c d 
 | threeEqual a b c && threeEqual a b d = True
 | otherwise = False

--- 3.17 ----
realRoot :: Float -> Float -> Float -> Bool
realRoot a b c 
 | (b**2 - 4*a*c) > 0 = True
 | otherwise = False

smallerRoot :: Float -> Float ->Float ->Float
smallerRoot a b c
 | not (realRoot a b c) = 1.0  
 | x < y = x
 | otherwise = y
 where
  x = ((-b) - sqrt((b**2 - 4*a*c))) / 2*a
  y = ((-b) + sqrt((b**2 - 4*a*c))) / 2*a 

largerRoot :: Float -> Float ->Float ->Float
largerRoot a b c
 | not (realRoot a b c) = 1.0  
 | x > y = x
 | otherwise = y
 where
  x = ((-b) - sqrt((b**2 - 4*a*c))) / 2*a
  y = ((-b) + sqrt((b**2 - 4*a*c))) / 2*a

 ----- Chapter 4 ---- 
 ---- 4.7 -----

add1::Int -> Int -> Int
add1 x y = x + y

mult1:: Int -> Int -> Int
mult1 x y
 | x == 0 || y == 0 = 0
 | otherwise = add y (mult1 (x-1) y)

--- 4.8 -----
getRoot :: Int -> Int -> Int
getRoot a b | b^2 > a = b - 1 |otherwise = getRoot a (b + 1)    

intSquareRoot :: Int -> Int
intSquareRoot a = getRoot a 1

---- 4.9 ----- 	
--- list generator ---

f1:: Int -> Int
f1 a = a `mod` 2 + hash a

f2:: Int -> [Int]
f2 n = map f1 [0,4,4,5,10,2,3,9,1]
--------------

max1:: Int -> Int -> Int
max1  x y 
 | x > y = x
 | otherwise = y

f3::[Int] -> Int
f3 [] = 0
f3 (x:xs) = max1 x (f3 xs)

---- 4.14 ------

isEven :: Int -> Bool
isEven 0 = True
isEven x = isOdd(x-1)


isOdd :: Int -> Bool
isOdd 0 = False
isOdd x = isEven(x-1)

powerOfTwo:: Int -> Int
powerOfTwo 0 = 1
powerOfTwo x 
 | isEven x = powerOfTwo (x `div` 2) ^ 2
 | otherwise =  (powerOfTwo (x `div` 2) ^ 2) * 2

 ------ Chapter 5 - 7 ----

 ----- 5.2 -----
orderTripple :: (Int,Int,Int) -> (Int,Int,Int)
orderTripple (a,b,c)
 | (a >= b && b >= c) = (a,b,c)
 | a >= b && b <= c && a > c= (a,c,b)
 | a <= b && a >= c = (b,a,c)
 | a <= b && a <= c && b >= c = (b,c,a)
 | a <= c && a >= b = (c,a,b)
 | a <= c && a <= b && b <= c = (c,b,a)

---- 5.10 ----- 


divisors :: Int -> [Int]
divisors 0 = []
divisors x = [n | n<-[1..x], x `mod` n == 0 ]

---- 5.11 ------ 
matches :: Int -> [Int] -> [Int]
matches x b = [n | n<-b , n == x]

elem1 :: Int -> [Int] -> Bool
elem1 x b 
 | matches x b == [] = False 
 |otherwise = True

 ---- 5.18 -------

shift :: ((x,y),z) -> (x,(y,z))
shift ((a,b),c) = (a,(b,c))

----- 5.22 ------ 
onSeparateLines :: [String] -> String
onSeparateLines [] = ""
onSeparateLines (x:xs) = x ++ "\n" ++ onSeparateLines xs

---- 5.23 -----

pushRight :: String -> String
pushRight [] = ""
pushRight x 
 | length x <= 12 = pushRight ("_|" ++ x)
 | otherwise = "_|" ++ x  