import Data.Char (ord,chr)
import Data.List
import Data.String
import System.IO
import Data.Hashable

main :: IO ()
main = return ()

insTuple::(Int,Int,Int,[Int])-> [(Int,Int,Int,[Int])] -> [(Int,Int,Int,[Int])]
insTuple n [] = [n]
insTuple n (x:xs)
 | (getSum n) < (getSum x) = n:x:xs
 | otherwise = x:insTuple n xs

listSort:: [(Int,Int,Int,[Int])]->[(Int,Int,Int,[Int])]
listSort xs = foldr insTuple [] xs

getSum::(Int,Int,Int,[Int]) -> Int
getSum (x,_,_,_) = x 

subLists::[Int] -> Int -> [(Int,Int,Int,[Int])]
subLists [] _ = []  
subLists xs i = [(sum (take y xs), i, i + (y-1), (take y xs)) | y <-[1..n]] ++ subLists (tail xs) (i+1)
 where 
  n = length xs 

toString2::(Int,Int,Int,[Int]) -> String
toString2 (x,y,z,l) = show x ++ "\t" ++ show y ++ "\t" ++ show z ++ "\t" ++ show l ++ "\n"

smallestKsets::[Int] -> Int -> IO ()
smallestKsets [] _ = error "Empty list"
smallestKsets xs n = putStr ("Size\ti\tj\tSublist\n" ++ concat (map toString2 (take n (listSort (subLists xs 1)))))     

