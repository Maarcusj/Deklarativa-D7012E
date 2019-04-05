import Atype(MyEq, (==),Visable , toString , size)

tjo :: MyEq a => a -> a -> Bool
tjo x y = x Atype.== y

test :: Visable a => a -> String
test x = toString x

test2 :: (Visable a) => a -> Int
test2 x = size x
