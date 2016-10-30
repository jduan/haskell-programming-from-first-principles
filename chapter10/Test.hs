module Test where

import Data.Time

myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = foldr (\x b -> f x || b) False xs

-- using foldl is bad!
myAny2 :: (a -> Bool) -> [a] -> Bool
myAny2 f xs = foldl (\b x -> f x || b) False xs

-- a function that takes the first 3 letters of each string in a list of
-- strings and concat the result into a final string
firstThree :: [String] -> String
firstThree [] = ""
firstThree xs = foldr ((++) . take 3) "" xs

data DatabaseItem
  = DbString String
  | DbNumber Integer
  | DbDate UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbNumber 1000
  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate items = map extractUTCTime $ filter isDbDate items
  where
    isDbDate (DbDate _) = True
    isDbDate _ = False
    extractUTCTime (DbDate utc) = utc

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber items = map extractNumber $ filter isDbNumber items
  where
    isDbNumber (DbNumber _) = True
    isDbNumber _ = False
    extractNumber (DbNumber n) = n

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent items = foldr max firstTime $ filterDbDate items
  where
    max t1 t2 =
      if t1 > t2
        then t1
        else t2
    times = filterDbDate items
    firstTime = head times

sumDb :: [DatabaseItem] -> Integer
sumDb items = sum $ filterDbNumber items

avgDb :: [DatabaseItem] -> Double
avgDb items = fromIntegral sum / fromIntegral len
  where
    sum = sumDb items
    len = length $ filterDbNumber items

factorial :: Int -> Integer
factorial n = lst !! n
  where
    lst :: [Integer]
    lst = scanl (*) 1 [1 .. toInteger n]

stops = "pbtdkg"

vowels = "aeiou"

stopVowelStop :: [String]
stopVowelStop =
  [ [stop1, vowel, stop2]
  | stop1 <- stops 
  , vowel <- vowels 
  , stop2 <- stops ]

startsWithA :: [String]
startsWithA =
  [ ['p', vowel, stop]
  | vowel <- vowels 
  , stop <- stops ]

-- avgWordLength x = sum (map length ws) / length ws
--   where
--     ws = words x
myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny3 :: (a -> Bool) -> [a] -> Bool
myAny3 f = foldr (\x y -> f x || y) False

myElem
  :: Eq a
  => a -> [a] -> Bool
myElem e = foldr (\x y -> x == e || y) False

myElem2
  :: Eq a
  => a -> [a] -> Bool
myElem2 e = myAny3 (== e)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x y -> f x : y) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter pred =
  foldr
    (\x y ->
        if pred x
          then x : y
          else y)
    []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f xs = squish $ map f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f [] = error "can't be empty list"
myMaximumBy f xs = foldr (\x y -> if f x y == GT then x else y) (head xs) xs
