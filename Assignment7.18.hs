
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


 

