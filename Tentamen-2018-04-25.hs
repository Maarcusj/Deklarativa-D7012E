import Data.Char (ord,chr)
import Data.List
import Data.String
import System.IO
import Data.Hashable
import System.Random

main :: IO ()
main = return ()

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

data Tree a = Node (Tree a)(Tree a)(Tree a)(a,Int) | Leaf deriving (Eq,Show, Ord)

t = (Node 
 		(Node
 			(Leaf)
 			(Leaf)
 			(Leaf)
 			('H',2)
 		)
 		(Leaf)
 		(Node
 			(Node
 				(Leaf)
 				(Leaf)
 				(Leaf)
 				('Y',3)
 			)
 			(Node
 				(Leaf)
 				(Leaf)
 				(Leaf)
 				('J',2)
 			)
 			(Leaf)
 			('Z',4)
 		)
 		('X',1)
 	)

extract::Int -> Tree a -> [a]
extract _ (Leaf) = []
extract i (Node x y z val)
 | snd val == i = [fst val] ++ extract i (x) ++ extract i (y) ++ extract i (z)
 | otherwise = [] ++ extract i (x) ++ extract i (y) ++ extract i (z)


game = 
 do 
  putStr ("Welcome to the Game!\nWhat secret number between 1 and 100 am I thinking of?!\n")
  n <- randomRIO ((1::Int),(10::Int))
  game' (show n,1)

game' (n,g) =
 do
  putStr ("Enter guess number " ++ show g ++ ": ")
  line <- getLine
  if(line == n) then
   do
    putStr ("Correct after " ++ show g ++ " guesses!\nThank's for playing!\n")
    return()
  else if (line < n) then
  	do                   
  	 putStr "Too low!\n"                              
  	 game' (n,g+1)
  else
  	do                   
  	 putStr "Too high!\n"                              
  	 game' (n,g+1)





