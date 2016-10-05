module MatchTypes where

import Data.List

i :: Num a => a
i = 1

f :: Float
f = 1.0

f2 :: Num a => a
f2 = 3

f3 :: Fractional a => a
f3 = 1.0

f4 :: RealFrac a => a
f4 = 1.0

freud :: Ord a => a -> a
freud x = x

freud' :: Int -> Int
freud' x = x

myX = 1 :: Int
sigmund :: Int -> Int
sigmund x = myX

-- jung :: Ord a => [a] -> a
jung :: [Int] -> Int
jung xs = head $ sort xs

mySort :: String -> String
-- you can assign a more generic function to a specialized one
mySort = sort

signifier :: String -> Char
signifier xs = head (mySort xs)
