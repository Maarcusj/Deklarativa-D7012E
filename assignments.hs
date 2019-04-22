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

duplicate :: String -> Int -> String
duplicate [] _ = ""
duplicate x y 
 | y <= 0 = ""
 | otherwise = x ++ duplicate x (y - 1) 


 ---- 5.24 ------ 

pushRight :: String -> String
pushRight [] = ""
pushRight x 
 | length x <= 12 = pushRight ("_|" ++ x)
 | otherwise = "_|" ++ x  


 ----- Chapter 6 ---- 

--- 7.2  ----
addTwo :: [Int] -> Int
addTwo [] = 0
addTwo (x:xs)  = x + addTwo xs

----- 7.3 ----- 

addTwo2 :: [Int] -> Int
addTwo2 x 
 | length x <= 0 = 0
 |otherwise = (head x) + addTwo2 (tail x)


 ---- 7.4 ---- 
product2 :: [Int] -> Int
product2 [] = 1
product2 (x:xs) = x * product2 xs

product3 :: [Int] -> Int
product3 xs = foldr (*) 1 xs

--- 7.5 ---- 

and2 , or2 ::[Bool] -> Bool
and2 xs = foldr (&&) True xs
or2 ys = foldr (||) False ys

------ 7.7 ------ 

unique' :: (Eq a) => [a] -> [a]
unique' [] = []
unique' (x:xs)
 | elem x xs = unique' (filter (/=x) xs)
 | otherwise = x:unique' (filter (/=x) xs)

----- 7.8 ---- 

reverse2::[t]->[t]
reverse2 [] = []
reverse2 (x:xs) = reverse2 xs ++ [x] 


unzip2::[(a,b)] -> ([a],[b])
unzip2 [] = ([],[])
unzip2 ((a, b):xs) = (a : fst x, b : snd y)
    where 
     y = unzip2 xs
     x = unzip2 xs


unzipThis::[(a,b)] -> ([a],[b])
unzipThis [] = ([], []) -- Defaults to a pair of empty lists, not null
unzipThis xs = (map fst xs, map snd xs)


---- 7.9 ----- 

ins::Int-> [Int] -> [Int]
ins n [] = [n]
ins n (x:xs)
 | n <= x = n:x:xs
 |otherwise = x:ins n xs

iSort:: [Int] -> [Int]
iSort xs = foldr ins [] xs


maxMin::[Int]->[Int]
maxMin xs = [head sorted] ++ [last sorted] 
 where 
  sorted = (iSort xs) 

maxMinFold :: Ord a => [a] -> (a, a)
maxMinFold (x:xs) = foldr (\x (tailMin, tailMax) -> (min x tailMin,max x tailMax)) (x,x) xs

maxMinFold2 :: Ord a => [a] -> (a, a)
maxMinFold2 (x:xs) = foldr maxtuple (x,x) xs

maxtuple::Ord a => a -> (a, a)-> (a, a)
maxtuple x (y,z) = (min x y, max x z) 

--- 7.14 ------ 

drop2::Int ->[a]->[a]
drop2 _ [] = []
drop2 n xs 
 | n > 0 = drop2 (n-1) (tail xs)
 | otherwise =  xs 

splitAt2::Int ->[a] -> ([a],[a])
splitAt2  _ [] = ([],[])
splitAt2 0 xs = ([], xs)
splitAt2 n (x:xs) = (x:y, z)
 where (y,z) = splitAt2 (n-1) xs 
  


---- 7.18 ----- 

sublist::String -> String -> Bool
sublist [] [] = True
sublist [] _ = True
sublist xs ys 
 | cut /= "" = sublist (tail xs) (tail ys)
 | otherwise = False
  where cut = cutAtChar (head xs) ys     


inString::Char -> String -> Bool
inString _ [] = False
inString x (y:ys)
 | x == y = True
 | otherwise = inString x ys


subsequence::String -> String -> Bool
subsequence [] [] = True 
subsequence _ [] = False
subsequence [] _ = False
subsequence xs ys
 | head xs == head cut = subsequence (tail xs) (tail ys)
 | otherwise = False
 where cut = cutAtChar (head xs) ys


cutAtChar::Char -> String -> String
cutAtChar _ [] = []
cutAtChar x (y:ys)
 | x == y = (y:ys)
 | otherwise = cutAtChar x ys

seqOrList::String->String->String
seqOrList xs ys
 | sublist xs ys && subsequence xs ys = "SubList and Subsequence"
 | sublist xs ys && not (subsequence xs ys) = "Sublist"
 | not (sublist xs ys) && not (subsequence xs ys) = "subsequence"
 | otherwise = "Nothing" 


---- Chapter 9 ------ 
---- 9.2 ----- 

myLength :: [a] -> Integer
myLength = foldr (\x -> (+) 1) 0



--- 9.4 --- 
g1::Int->Int
g1 x = x + 1 

f4::Int->Int
f4 x = x - x  

g2:: Int -> Int
g2 x = x * 2   

gf2::[Int] -> [Int]
gf2 x = map g1 (map g2 x) 

gf::[Int] -> [Int]
gf x = map (g2 . g1) x

---- 9.6 ---- 

squareList::[Int] -> [Int]
squareList xs = map (\x -> x^2) xs

sumSquare::[Int] -> Int
sumSquare xs = foldr (+) 0 (map (\x -> x^2) xs)

greaterZero::[Int] -> Bool
greaterZero xs
 | myLength (filter (\x -> x <= 0) xs) > 0 = False
 | otherwise = True  

--- 9.7 ----

minFunc::(Int->Int) -> Int -> Int
minFunc f n = foldr min (head list) list
 where list = map f [0..n] 

allEqual :: (Int -> Int) -> Int -> Bool
allEqual f n = and (map (==(f 0)) (map f [0..n]))


---- 9.9 -----

iter::Int -> (a->a) -> a -> a
iter 0 f x = x
iter n f x = iter (n-1) f (f x)

--- 9.10 ----

multTwo::Int->Int
multTwo 0 = 1
multTwo x = double 1 * multTwo (x-1) 

---- 9.11 ----- 

sum1::[Int] -> Int
sum1 [] = 0
sum1 xs = foldr (+) 0 (map (\x -> x^2) xs)

---  9.16 ---- 

filterFirst :: (a -> Bool) -> [a] -> [a]
filterFirst p [] = []
filterFirst p (x:xs) 
 |p x = x : filterFirst p xs
 |otherwise = xs 

---- 9.17 -----

filterLast :: (a -> Bool) -> [a] -> [a]
filterLast p list = reverse (filterFirst p (reverse list))


--- Chapter 10 -------
--- 10.3 -------

t :: Int -> Int
t x = x + 1

t2 :: Int -> Int
t2 x = x + 1

t3 :: Int -> Int
t3 x = x + 1

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
 

---- chapter 12 ----- 


-- 12.2 ---- 

numEqual::Eq a => [a] -> a -> Int
numEqual [] _ = 0
numEqual xs x = length (filter (==x) xs) 

--- 12.3 ----

onLookupFirst::Eq a => [(a,b)] -> a -> b
onLookupFirst xs t = snd (head (filter (\(x,y)-> x==t) xs))

onLookupSecond::Eq a => [(a,b)] -> a -> (b,a)
onLookupSecond xs t = (snd tup , fst tup)
 where tup = head (filter (\(x,y)-> x==t) xs)

--- 12.4 , 12.5---- 

class Visible a where
 toString :: a -> String
 size:: a -> Int

instance Visible Char where
 size _ = 1 
 toString y = [y]  

instance Visible Int where
 toString 0 = "0"
 toString 1 = "1"
 toString 2 = "2"
 toString 3 = "3"
 toString 4 = "4"
 toString 5 = "5"
 toString 6 = "6"
 toString 7 = "7"
 toString 8 = "8"
 toString 9 = "0"
 toString n = foldr1 (++) (map toString (intList n))
 size x = x  

instance (Visible a, Visible b)  => Visible (a, b) where
 toString (a,b)  = toString a ++ toString b
 size _ = 10  

instance Visible Bool where
  toString True  = "True"
  toString False = "False"
  size _         = 1

intList :: Int -> [Int]
intList n
 |(mod n 10) /= n = intList (div n 10) ++ [mod n 10]
 |otherwise = [n]

sizeTuple::(Visible a, Visible b) => (a,b) -> Int
sizeTuple (x,y) = size (x,y)

stringTuple::(Visible a, Visible b) => (a,b) -> String
stringTuple (x,y) = toString (x,y)

toSize :: Visible a => a -> Int 
toSize x = size x 

toStr :: Visible a => a -> String 
toStr x = toString x

comparer :: (Visible a, Visible a1) => a -> a1 -> Bool 
comparer  x y = size x <= size y 


---- 12.8 ----

---- 12.10 --- 
showBoolFun :: (Bool -> Bool) -> String
showBoolFun f = "True:\t" ++ (toString (f True)) ++ "\nFalse:\t" ++ (toString (f False)) ++ "\n"


showBoolFunGen :: (a -> String) -> (Bool -> a) -> String
showBoolFunGen am f = "True:\t" ++ (am (f True)) ++ "\n" ++ "False:" ++ "\t" ++ (am (f False)) ++ "\n"

boolFunc::Bool -> Bool
boolFunc x = x

---- Chapter 13 ----

--- Skippa , fett tråkigt med type checking
-- Gör senare om tid finns

---- Chapter 14 --- 


--- 14.1 ---

data Temp = Cold | Hot
 deriving (Eq , Ord , Show , Read)

data Season = Spring | Summer | Autumn | Winter
 deriving (Eq , Ord , Show , Read) 

data Shape = Circle Float | Rectangle Float Float | Triangle Float Float Float
 deriving ( Show , Read)

weather:: Season -> Temp
weather x 
 | x == Summer = Hot
 | otherwise = Cold

--- 14.4 ----

shapePerimeter:: Shape -> Float
shapePerimeter (Circle r) = 2 * pi * r
shapePerimeter (Rectangle x y) = 2*(x+y)
shapePerimeter (Triangle x y z) = x + y + z

--- 14.5 ----- 

isRound:: Shape -> Bool
isRound (Circle _) = True 
isRound (Rectangle _ _ ) = False
isRound (Triangle _ _ _) = False

area::Shape -> Float
area (Circle r) = pi * r^2
area (Rectangle a b) = a * b
area (Triangle x y z) = sqrt (p*(p-x)*(p-y)*(p-z))
 where p = (x + y + z)/2

--- 14.6 ----
isRegular :: Shape -> Bool 
isRegular (Circle _) = True
isRegular (Rectangle x y)
 |x==y = True
 |otherwise = False
isRegular (Triangle a b c)
 |a==b && b==c && c==a = True
 |otherwise = False

-- 14.8 ----

instance Eq Shape where
  (Circle a) == (Circle b) = a==b && (a>0 && b>0)
  (Rectangle h w) == (Rectangle h' w') = ((h==h' && w==w') || (h==w' && w==h'))
  (Triangle a b c) == (Triangle d e f) = (([a, b, c] == [d, e, f]) || ([a, b, c] == [e, d, f]) || ([a, b, c] == [f, e, d]))
  

--- 14.9 , 14.10 --- 
type Point = (Float,Float)

data NewShape = CircleP Float Point| RectangleP Float Float Point| TriangleP Float Float Float Point
 deriving ( Eq , Ord , Show , Read)

move:: Float -> Float -> NewShape -> NewShape
move x y (CircleP r p) = CircleP r (newPoint x y p)
move x y (RectangleP h w p) = RectangleP h w (newPoint x y p)
move x y (TriangleP s1 s2 s3 p) = TriangleP s1 s2 s3 (newPoint x y p)


newPoint::Float -> Float -> Point -> Point
newPoint x y (p1,p2) = (p1 + x, p2 + y) 

--- 14.15 ----
data Expr = Lit Int |Add Expr Expr | Sub Expr Expr

eval :: Expr -> Int
eval (Lit n) = n
eval  (Add e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)

sizeExpr::Expr -> Int
sizeExpr (Lit _ ) = 0
sizeExpr (Add e1 e2) = 1 + sizeExpr e1 + sizeExpr e2
sizeExpr (Sub e1 e2) = 1 + sizeExpr e1 + sizeExpr e2

data Tree a = Nil | Node a (Tree a) (Tree a)
 deriving (Eq , Ord , Show , Read)

depth::Tree a -> Int
depth Nil = 0
depth (Node n t1 t2) = 1 + max (depth t1) (depth t2)


--- 14.28 ---

-- Chapter 16 ---

