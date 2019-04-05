module Shapes2(Shapes, (Shapes.==)) where

data Shape = Circle Float 
	| Rectangle Float Float 
	| Triangle Float Float Float
	deriving (Eq,Ord,Show,Read) 

class Shapes a where
	(==) :: a -> a-> Bool

instance Shapes Shape where
	a == b = a Prelude.== b
