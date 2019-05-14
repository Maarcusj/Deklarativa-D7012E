import Data.Char (ord,chr)
import Data.List
import Data.String
import System.IO
import Data.Hashable
import System.Random

main :: IO ()
main = return ()

--append2::[a]->[a]->[a]
--append2 [] ys = ys
--append2 (x:xs) (y:ys) = x:xs append2 ys

append::[a]->[a]->[a]
append [] ys = ys
append (x:xs) ys = x:(append xs ys)

--(++) :: [a] -> [a] -> [a]
--(++) [] ys = ys
--(++) (x:xs) ys = x : xs ++ ys

concat2::[[a]]->[a]
concat2 [] = []
concat2 (x:xs) = append (x) (concat2 xs)