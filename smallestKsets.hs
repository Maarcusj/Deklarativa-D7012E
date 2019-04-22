import Data.Char (ord,chr)
import Data.List
import Data.String
import System.IO
import Data.Hashable

main :: IO ()
main = return ()




sortSubList::[[Int]] -> [(Int,Int,Int,[Int])]
sortSubList [] = []
sortSubList (x:xs) =   

makeSubLists::[Int] -> [(Int,Int,Int,[Int])]
makeSubLists [] = []
makeSubLists xs = (subLists xs) ++ (subLists (tail xs)) 

subLists::[Int] -> [[Int]]
subLists [] = []  
subLists xs  = [xs] ++ (subLists (init xs))

