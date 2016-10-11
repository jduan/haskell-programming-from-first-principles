module Exercises where

import Data.Char

uppercaseOnly :: String -> String
uppercaseOnly = filter isUpper

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs

firstLetterInCap :: String -> Char
firstLetterInCap s = toUpper $ head s

firstLetterInCap2 s = (toUpper . head) s

firstLetterInCap3 = toUpper . head


myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem e (x:xs) = e == x || myElem e xs

myElem2 e = myAny (== e)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

myReverse2 [] = []
myReverse2 (x:xs) = go (x:xs) []
    where go [] acc = acc
          go (x:xs) acc = go xs $ x:acc

squish :: [[a]] -> [a]
squish [[]] = []
squish ([]:xs) = squish xs
squish ((x:xs):ys) = x : squish (xs:ys)

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f xs = squish $ map f xs

squishAgain :: [[a]] -> [a]
squishAgain xss = squishMap id xss

findElementBy :: (a -> a -> a) -> [a] -> a
findElementBy _ [] = error "empty list"
findElementBy f lst@(x:xs) = go lst x
    where go [] current = current
          go (x:xs) current = go xs (f current x)

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy cmp [] = error "empty list"
myMaximumBy cmp lst = findElementBy f lst
    where f x y = case cmp x y of
                       GT -> x
                       _ -> y

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy cmp [] = error "empty list"
myMinimumBy cmp lst = findElementBy f lst
    where f x y = case cmp x y of
                       LT -> x
                       _ -> y


myMaximum :: (Ord a) => [a] -> a
myMaximum xs = myMaximumBy compare xs

myMinimum :: (Ord a) => [a] -> a
myMinimum xs = myMinimumBy compare xs
