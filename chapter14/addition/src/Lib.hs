module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

sayHello :: IO ()
sayHello = putStrLn "hello!"

mySum a b = a + b

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
    where go n d count
              | n < d = (count, n)
              | otherwise = go (n - d) d (count + 1)

multiply :: (Integral a) => a -> a -> a
multiply x y
    | x == 0 = 0
    | x < 0 = negate (go (-x) y 0)
    | otherwise = go x y 0
    where go x y product
              | x == 0 = product
              | otherwise = go (x - 1) y (product + y)
