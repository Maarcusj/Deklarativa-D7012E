module Atype(MyEq, (Atype.==), Visable , toString , size) where
class MyEq a where
	(==) :: a -> a -> Bool

instance MyEq Integer where
	a == b = a Prelude.== b

	
class Visable a where 
	toString :: a -> String 
	size :: a -> Int 

newtype Wrapper = Wrapper String 	

instance Visable Integer where
	toString _ = "Not yet available"
	size x = fromIntegral(x) * 2


--instance Visable Integer where
--	toString x = "Not yet available"
--	size x = 3
instance (Visable a, Visable b) => Visable (a,b) where
	toString _ = "Not yet available"
	 

instance Visable Bool where
	toString True = "True"
	toString False = "False"
	size _ = 1



--instance (Visable a, Visable b) => Visable (a,b) where
--	toString  (x,y) = map   

	

		
			

