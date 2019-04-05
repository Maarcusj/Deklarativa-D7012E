import Data.Char (ord,chr)
import Data.List
import Data.String
import System.IO

----- TEST CASE -------
-- smallestKsets [x*(-1)^x | x <- [1..100]] 15
-- smallestKsets [24,-11,-34,42,-24,7,-19,21] 6

-----------------------

-- Sum of all integers in list
sumOfList :: [Int] -> Int
sumOfList [] = 0
sumOfList (x:xs) = x + sumOfList xs 
  
-- Sum of list in tuple, last element  
getListSumFromTuple :: (Int,Int,Int,[Int]) -> Int
getListSumFromTuple (_,_,_,x) = sumOfList x


--makes sublist of list of integers
sublist :: [Int] -> Int -> [(Int,Int,Int,[Int])]
sublist [] a = [] 
sublist (x:xs) a = [(sumOfList (take z (x:xs)) , a , (a + (z-1)) ,take z (x:xs)) | z<-[1..y]] ++ (sublist xs (a+1))
	where 
		y = length (x:xs)

-- smallest subsets list and num of sublist
smallestKsets :: [Int] -> Int -> IO ()
smallestKsets [] y = error "Empty list"
smallestKsets x y = printThis (listPrint(listToString (take y (insertSort (sublist (x) 1))))) 


-- sort list of sublists
insertSort :: [(Int,Int,Int,[Int])] -> [(Int,Int,Int,[Int])]
insertSort [] = []
insertSort (x:xs) = insertList x (insertSort xs)  

insertList :: (Int,Int,Int,[Int]) -> [(Int,Int,Int,[Int])] -> [(Int,Int,Int,[Int])]
insertList 	x [] = [(x)]
insertList x (y:ys)  
	| getListSumFromTuple (x) < getListSumFromTuple (y) = x:(y:ys)
	| otherwise = y:insertList x ys 


--- print functions
printThis :: String -> IO ()
printThis x = putStr x

listPrint :: String -> String 
listPrint x = "Size\ti\tj\tSublist\n" ++ x

listToString :: [(Int,Int,Int,[Int])] -> String
listToString  [] = []
listToString (x:xs) = tupleToString x ++ "\n" ++ listToString xs
 
tupleToString :: (Int,Int,Int,[Int]) -> String
tupleToString (a,b,c,d) =  show a ++ "\t" ++ show b ++ "\t" ++ show c ++ "\t" ++ show d 
 
--sortList :: [(Int,Int,Int,[Int])] -> [(Int,Int,Int,[Int])]
--sortList [] = [(0,0,0,[])]
--sortList (x:xs) = [a | a<-xs , getListSumFromTuple a<=c] ++ [x] ++ [ b | b<-xs, getListSumFromTuple b > c]
--	where 
--		c = getListSumFromTuple x

--getSubLists :: [[Int]] -> [[Int]]
--getSubLists [] = []
--getSubLists x 
--		|n >= 0 = takeList x ++ getSubLists (init x)
--		|otherwise = []
--		where 
--			n = (length x ) - 1  

--intlist:: [Int] -> [[Int]]
--intlist (x:xs) = [[a] | a<-(x:xs)] 

--getSubLists2 :: [[Int]] -> [[Int]]
--getSubLists2 [] = []
--getSubLists2 x 
--		|n >= 0 = takeList2 x ++ getSubLists2 (init x)
--		|otherwise = []
--		where n = (length x ) - 1 


--takeList2 :: [[Int]] -> [[Int]]
--takeList2 [] = [] 
--takeList2 ((x:xs):xss) 
--		| y > 0 = [concat (take y ((x:xs):xss))] ++ takeList2 (xss)
--		| otherwise = []
--		where y = length ((x:xs):xss)

--minSublist :: [Int] -> [Int] -> Int
--minSublist [] []= 0
--minSublist x y 
--		| sumOfList (x) < sumOfList (y) = sumOfList (x)
--		|otherwise = sumOfList (y)  

--smallestSubSet :: [[Int]] -> [[Int]]-> [[Int]]
--smallestSubSet [] [] = [] 
--smallestSubSet (x:xs) (y:ys) 
--		|     		

---smallestSubSet2 :: [[Int]] -> [[Int]]
--smallestSubSet2 [] = [] 
--smallestSubSet2 (x:xs) = [[ sumOfList(x) | x<-(x:xs) ]]  	

--nextSubList :: [[Int]] -> [Int]
--nextSubList [] = []
--nextSubList x = head x 	

--doublelistsOfList :: [Int] -> [[Int]]
--doublelistsOfList [] = []
--doublelistsOfList (x:xs) = [[ a | a<-(x:xs)] ]

-- Length , i , j , sublist[] ---- 
--infoList :: [Int] -> [(Int,Int,Int,[Int])]
--infoList [] = []
--infoList (x:xs) 
--	| l == 1 = [(1,0,0,[x])]
--	| otherwise = [, , , [ ]]
--	where 
--		l = length (x:xs) 



 


