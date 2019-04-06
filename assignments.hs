import Data.Char (ord,chr)
import Data.List
import Data.String
import System.IO

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