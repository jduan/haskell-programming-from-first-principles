module Exercises where

tensDigit :: Integral a => a -> a
tensDigit x = d
    where (xLast, _) = x `divMod` 10
          (_, d) = xLast `divMod` 10

hundredsDigit x = d
    where (xLast, _) = x `divMod` 100
          (_, d) = xLast `divMod` 10

foldBool :: a -> a -> Bool -> a
foldBool x y True = x
foldBool x y False = y

foldBool2 x y b = case b of
                       True -> x
                       False -> y

foldBool3 x y b
    | b = x
    | otherwise = y

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)

roundTrip :: (Show a, Read a) => a -> a
-- roundTrip a = read (show a)
roundTrip a = read $ show a

roundTrip2 :: (Show a, Read b) => a -> b
roundTrip2 x = read $ show x

main = do
    print (roundTrip2 4 :: Integer)
    print (id 4)
