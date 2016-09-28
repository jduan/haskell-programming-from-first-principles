module Exercises where

awesome = ["Papuchon", "curry", ":)"]
alsoAwesome = ["Quake", "The Simons"]
allAwesome = [awesome, alsoAwesome]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

myAbs :: Integer -> Integer
myAbs x = if x >= 0 then x else -x

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f t1 t2 = ((snd t1, snd t2), (fst t1, fst t2))

-- pattern matching
f2 :: (a, b) -> (c, d) -> ((b, d), (a, c))
f2 (a, b) (c, d) = ((b, d), (a, c))

x = (+)
f3 xs = w `x` 1
    where w = length xs

-- lambda function
identity = \x -> x

myHead :: [a] -> a
myHead = \(x:xs) -> x

f4 :: (a, b) -> a
f4 (a, b) = a
