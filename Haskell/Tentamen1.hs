module Tentamen1 where

import Data.Char (ord,chr)
import Data.List
import Data.String
import System.IO

---- 1 a------
newTail :: [a] -> [a]
newTail [] = error "empty list"
newTail (x:xs) = xs

newInit :: [a] -> [a]
newInit [] = error "empty list"
newInit [x] = [] 
newInit (x:xs) =  x : newInit xs 

----- 1 b ---------

suffixes :: [a] -> [[a]]
suffixes [] = [] 
suffixes (x:xs) =  (x:xs) : suffixes xs  

prefixes :: [a] -> [[a]]
prefixes [] = []
prefixes x = x : prefixes (newInit x )

----- 2a ---------

data Tree a = Node (Tree a) (Tree a) | Leaf a deriving(Show,Ord,Eq) 

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
	
sumTree :: Tree Int -> Int
sumTree (Leaf x) = x
sumTree (Node left right) = sumTree left + sumTree right 

----- 3 a - h --------

isLetter 'a' = True
isLetter _ = False

-- "x" [Char]

-- (1,'x',[True]) == (Int, Char , [Bool] )

-- ["x":[]] == [[[Char]]]

-- not.not.isLetter :: Char -> Bool

-- map not :: [Bool] -> [Bool]

-- (+1).(0<) , Num and Bool can not be combined 

--  \x -> x +1 :: Num a => a -> a

-- [1,'2',"3"] , error cant match Char with [Char] 

------ 4 --------

calculator :: IO()






