class Ar a where 
	(==) :: a->a-> Bool

numEqual :: Ar a =>[a] -> a -> Int
numEqual [] b = 0
numEqual x b = length ([a | a<-x, a Ar.== b]) 