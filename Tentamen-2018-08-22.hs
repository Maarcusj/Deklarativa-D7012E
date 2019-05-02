import Data.Char (ord,chr)
import Data.List
import Data.String
import System.IO
import Data.Hashable
import System.Random

main :: IO ()
main = return ()

fb [] ys = ys
fb (x:xs) ys = x:(fb xs ys)

o ::(b->c)->(a->b)->(a->c)
o f g = \x -> f (g x)    


data T a = Node (T a) (T a) a | Leaf a deriving (Show,Ord, Eq)

t = 
 Node 
 (Node(Node(Leaf 1)(Leaf 2)(4))(Node(Leaf 3)(Leaf 4)(5))(2)) 
 (Node(Leaf 5)(Leaf 6)(3)) 
 (1)

transform::T a -> (a -> a) -> T a
transform (Leaf a) f = Leaf (f a)
transform (Node x y z) f = (Node (transform (x) (f)) (transform (y) (f)) (f z))

g n = if n < 3 then n else g(n-1) + g(n-2) + g(n-3)


valid::[[Int]] -> (Int,Int) ->Bool
valid [] _ = False
valid xs (x,y)
 | length(head xs) >= x && length xs >= y = True
 | otherwise = False

a::[[Int]] -> (Int,Int) -> Int
a xs (x,y) = head(drop (x-1) (head (drop (y-1) xs)))

m::[[Int]]
m = [[8,8,8,8,8,8,8,8,8,8],[8,0,8,0,0,0,0,0,0,8],[8,0,8,0,8,8,0,8,8,8],[8,0,8,0,8,8,0,0,0,8],[8,0,0,0,0,0,0,8,0,8],[8,0,8,8,8,0,8,0,0,8],[8,0,8,0,8,0,8,0,8,8],[8,0,8,0,8,0,0,0,0,8],[8,8,8,8,8,8,8,1,8,8]]

escapeable::[[Int]] -> (Int,Int) -> Bool
escapeable xs (x,y)
 | valid xs (x,y) == False = False
 | (a xs (x,y)) == 0 = escapeable temp (x,y+1) || escapeable temp (x,y-1) || escapeable temp (x-1,y) || escapeable temp (x+1,y) 
 | (a xs (x,y)) == 1 = True
 | otherwise = False
 where
  h = (take (y-1) xs)
  t = (drop y xs)
  l = head (drop (y-1) xs)
  temp = h ++ (replaceX l x) ++ t

replaceX::[Int] -> Int -> [[Int]]
replaceX xs n = [(take (n-1) xs) ++ [8] ++ (drop n xs)]

calc :: IO ()
calc= do
 putStr "Enter number of positive integers to add: "
 line <- getLine
 sum <- calc' (read line) 0
 putStr ("Total sum: " ++ (show sum) ++ "\n")

calc' :: Int -> Int -> IO Int
calc' 0 sum = return sum
calc' n sum = do
 putStr "Enter an integer: "
 next <- getLine
 calc' (n-1) (sum + read next)