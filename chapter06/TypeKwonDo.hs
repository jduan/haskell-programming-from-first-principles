module TypeKwonDo where

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk aToB a b = aToB a == b

-- Hint: use some arithmetic operation to combine values of type 'b'.
arith :: Num b => (a -> b) -> Integer -> a -> b
arith aToB i a = aToB a * i
