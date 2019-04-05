import Data.Char (ord,chr)
import Data.List

main :: IO ()
main = return ()

double :: Int -> Int
double n = n * 2

add :: Int -> Int -> Int
add x y = x + y

sub :: Int -> Int -> Int
sub x y = x - y

mult :: Int -> Int -> Int
mult x y = x * y 

sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = add x (sumList xs)


