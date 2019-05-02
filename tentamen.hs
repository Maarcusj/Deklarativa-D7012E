import Data.Char (ord,chr)
import Data.List
import Data.String
import System.IO
import Data.Hashable

main :: IO ()
main = return ()

data Tree a = Node (Tree a ) (Tree a) | Leaf a deriving (Show, Ord, Eq)

t =
  Node
    (Node
      (Leaf 1)
      (Node
        (Node
          (Leaf 2)
          (Leaf 3)
        )
        (Leaf 4)
      )
    )
    (Node
      (Leaf 5)
      (Node
        (Leaf 6)
        (Node
          (Leaf 7)
          (Leaf 8)
        )
      )
    )

sumTree::Num a => Tree a -> a
sumTree (Leaf a) = a
sumTree (Node x y) = (sumTree x) + (sumTree y)

calculator:: IO ()
calculator = calculator' []
calculator' a =
 do putStr "Enter number:"
    line <- getLine
    if (line == []) then
     do
       putStr ("Numbers entered: " ++ format a ++ "\nAccumulated sum: " ++ show (calc a) ++ "\nSum reset.\n")
    else calculator' (a ++ [(myFunc line)])

format :: [Int] -> String
format [] = "(none)"
format [a] = show a
format (x:xs) = show x ++ ", " ++ format xs
    
calc :: [Int] -> Int
calc [] = 0
calc (x:xs) = x + calc xs

myFunc :: String -> Int
myFunc s = read s


map2::(a->b)->[a]->[b]
map2 _ [] = []
map2 f xs = [f x | x <- xs]

filter'::(a->Bool) -> [a] -> [a]
filter' _ [] = []
filter' f xs = [x | x <- xs , f x] 

evenCubeSum::Int->Int
evenCubeSum n = foldr (\s z->s + z) 0 (map (\x -> x^3) (filter (\t -> t `mod` 2 == 0) [2..n]))

evenCubeSum1 n = foldr (\x y -> x*x*x+y) 0 [2,4..n]

evenCubeSum2 n = foldr (\x y -> if x `mod` 2 == 0 then x*x*x+y else y) 0 [2,3..n]

evenCubeSum3 n = foldr (+) 0 ( (map (^3).filter ((==0).(`mod` 2))) [2..n])

t2 ls = foldr (\x y -> if x `mod` 2 == 0 then x*x*x+y else y) 0 [2,3.. 2*(ls `div`2)]


isOrdered::Ord a => [a] -> Bool
isOrdered [] = True
isOrdered [x] = True
isOrdered (x:xs)
 | x <= head xs = isOrdered xs
 | otherwise = False 

compress::[Int] -> [(Int,Int)]
compress [] = []
compress [x] = [(x,x)]
compress xs = [(head xs, last y)] ++ compress (drop t xs) 
 where
  y = (subList (xs) (head xs))
  t = length y

subList::[Int]-> Int ->[Int]
subList [] _ = []
subList (x:xs) n 
 | n == x || n+1 == x = [x] ++ subList xs x
 | otherwise = []

mkList::Int->Int->[Int]
mkList x y = [x] ++ mkList (x+y) (y) 

