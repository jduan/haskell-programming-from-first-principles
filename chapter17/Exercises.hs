module Exercises where

import Data.List (elemIndex)

-- Exercise 1
added :: Maybe Integer
added = (+ 3) <$> (lookup 3 $ zip [1 .. 3] [4 .. 6])

-- Exercise 2
y :: Maybe Integer
y = lookup 3 $ zip [1 .. 3] [4 .. 6]

z :: Maybe Integer
z = lookup 2 $ zip [1 .. 3] [4 .. 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

-- Exercise 3
s :: Maybe Int
s = elemIndex 3 [1 .. 5]

t :: Maybe Int
t = elemIndex 4 [1 .. 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> s <*> t

-- Exercise 4
xs = [1, 2, 3]

ys = [4, 5, 6]

x1 :: Maybe Integer
x1 = lookup 3 $ zip xs ys

y1 :: Maybe Integer
y1 = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = fmap sum $ (,) <$> x1 <*> y1
