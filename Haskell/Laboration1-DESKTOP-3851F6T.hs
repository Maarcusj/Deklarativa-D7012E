import Data.Char (ord,chr)
import Data.List
import Data.String
import System.IO


sumOfList :: [Int] -> Int
sumOfList [] = 0
sumOfList (x:xs) = x + sumOfList xs 

sortList :: [(Int,Int,Int,[Int])] -> [(Int,Int,Int,[Int])]
sortList [] = [(0,0,0,[])]
sortList (x:xs) = [a | a<-xs , getListFromTuple a<=c] ++ [x] ++ [ b | b<-xs, getListFromTuple b > c]
	where 
		c = getListFromTuple x  

getListFromTuple :: (Int,Int,Int,[Int]) -> Int
getListFromTuple (_,_,_,x) = sumOfList x

sublist :: [Int] -> Int -> [(Int,Int,Int,[Int])]
sublist [] a = [] 
sublist (x:xs) a = [(z, a , (a + (z-1)) ,take z (x:xs)) | z<-[1..y]] ++ (sublist xs (a+1))
	where y = length (x:xs)



smallestKsets :: [Int] -> Int -> [(Int,Int,Int,[Int])]
smallestKsets [] y = error "Empty list"
smallestKsets x y = take y (sortList (sublist (x) 1)) 


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



 


