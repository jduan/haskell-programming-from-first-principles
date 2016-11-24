module Exercises where

import Data.Char

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe s = Just s

replaceThe :: String -> String
replaceThe sentence = unwords $ map (transform . notThe) (words sentence)
  where
    transform Nothing = "a"
    transform (Just s) = s

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel sentence = sum $ map theVowel pairs
  where
    pairs = zip ws (tail ws)
    ws = words sentence
    theVowel ("the", word) =
      if head word `elem` ['a', 'e', 'i', 'o', 'u']
        then 1
        else 0
    theVowel (_, _) = 0

countVowels :: String -> Int
countVowels sentence =
  sum $
  map
    (\l ->
        if isVowel l
          then 1
          else 0)
    sentence
  where
    isVowel ch = toLower ch `elem` "aeiou"

newtype Word' =
  Word' String
  deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord sentence =
  if numOfVowels > numOfConsonants
    then Nothing
    else Just (Word' sentence)
  where
    numOfVowels = countVowels sentence
    numOfConsonants = length sentence - numOfVowels

-- natural numbers
data Nat
  = Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ a) = natToInteger a + 1

integerToNat :: Integer -> Maybe Nat
integerToNat 0 = Just Zero
integerToNat 1 = Just (Succ Zero)
integerToNat i =
  if i > 0
    then Just (f m)
    else Nothing
  where
    m = integerToNat (i - 1)
    f (Just n) = Succ n

-- small library for Maybe
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b f Nothing = b
mayybee b f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing = a
fromMaybe a (Just b) = b

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes = foldr f []
  where
    f Nothing xs = xs
    f (Just a) xs = a : xs

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe (Nothing:xs) = Nothing
flipMaybe (Just a:xs) = f rest
  where
    rest = flipMaybe xs
    f (Just b) = Just (a : b)
    f Nothing = Nothing

-- small library for Maybe
lefts' :: [Either a b] -> [a]
lefts' = foldr f []
  where
    f (Left a) acc = a : acc
    f (Right b) acc = acc

rights' :: [Either a b] -> [b]
rights' = foldr f []
  where
    f (Left a) acc = acc
    f (Right b) acc = b : acc

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' eithers = (lefts' eithers, rights' eithers)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Left a) = Nothing
eitherMaybe' f (Right b) = Just $ f b

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f1 f2 (Left a) = f1 a
either' f1 f2 (Right b) = f2 b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)

myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = go (f b)
  where
    go Nothing = []
    go (Just (a, b')) = a : myUnfoldr f b'

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\b -> Just (b, f b)) x

-- binary tree
data BinaryTree a
  = Leaf
  | Node (BinaryTree a)
         a
         (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f a = go (f a)
  where
    go Nothing = Leaf
    go (Just (a1, b, a2)) = Node (unfold f a1) b (unfold f a2)


treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold f 0
  where f x =
          if x == n then Nothing
                    else Just (x+1, x, x+1)
