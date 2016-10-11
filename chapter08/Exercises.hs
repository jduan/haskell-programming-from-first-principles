module Exercises where

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

mySum :: (Eq a, Num a) => a -> a
mySum n = go 0 n
    where go sum n
              | n == 0 = sum
              | otherwise = go (sum + n) (n - 1)

multiply :: (Integral a) => a -> a -> a
multiply x y
    | x == 0 = 0
    | x < 0 = negate (go (-x) y 0)
    | otherwise = go x y 0
    where go x y product
              | x == 0 = product
              | otherwise = go (x - 1) y (product + y)


mc91 :: (Ord t, Num t) => t -> t
mc91 n
    | n > 100 = n - 10
    | otherwise = mc91 $ mc91 (n + 11)
