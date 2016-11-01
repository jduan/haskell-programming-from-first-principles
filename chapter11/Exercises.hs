module Exercises where

import Data.Char

data FlowerType
  = Gardenia
  | Daisy
  | Rose
  | Lilac
  deriving (Show)

type Gardener = String

data Garden =
  Garden Gardener
         FlowerType
  deriving (Show)

data Garden2
  = GardeniaGarden Gardener
  | DaisyGarden Gardener
  | RoseGarden Gardener
  | LilacGarden Gardener

data OperatingSystem
  = GNU
  | OpenBSD
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgrammingLanguage
  = Haskell
  | Ada
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer = Programmer
  { os :: OperatingSystem
  , lang :: ProgrammingLanguage
  } deriving (Eq, Show)

allOperatingSystems = [GNU, OpenBSD, Mac, Windows]

allLanguages = [Haskell, Ada, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers =
  [ Programmer
   { os = os
   , lang = lang
   }
  | os <- allOperatingSystems 
  , lang <- allLanguages ]

newtype Name =
  Name String
  deriving (Show)

newtype Acres =
  Acres Int
  deriving (Show)

data FarmerType
  = DairyFarmer
  | WheatFarmer
  | SoybeanFarmer
  deriving (Show)

data Farmer =
  Farmer Name
         Acres
         FarmerType
  deriving (Show)

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer (Farmer _ _ _) = False

data FarmerRec = FarmerRec
  { name :: Name
  , acres :: Acres
  , farmerType :: FarmerType
  } deriving (Show)

isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmer =
  case farmerType farmer of
    DairyFarmer -> True
    _ -> False

data Automobile
  = Null
  | Car { make :: String
        , model :: String
        , year :: Integer}
  deriving (Eq, Show)

data Quantum
  = Yes
  | No
  | Both
  deriving (Eq, Show)

-- function type is exponential
-- a -> b
-- b^a
convert1 :: Quantum -> Bool
convert1 Yes = True
convert1 No = True
convert1 Both = True

convert2 :: Quantum -> Bool
convert2 Yes = True
convert2 No = True
convert2 Both = False

convert3 :: Quantum -> Bool
convert3 Yes = True
convert3 No = False
convert3 Both = True

convert4 :: Quantum -> Bool
convert4 Yes = True
convert4 No = False
convert4 Both = False

convert5 :: Quantum -> Bool
convert5 Yes = False
convert5 No = True
convert5 Both = True

convert6 :: Quantum -> Bool
convert6 Yes = False
convert6 No = True
convert6 Both = False

convert7 :: Quantum -> Bool
convert7 Yes = False
convert7 No = False
convert7 Both = True

convert8 :: Quantum -> Bool
convert8 Yes = False
convert8 No = False
convert8 Both = False

data BinaryTree a
  = Leaf
  | Node (BinaryTree a)
         a
         (BinaryTree a)
  deriving (Eq, Ord, Show)

insert'
  :: Ord a
  => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree f Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

-- vigenere cipher
-- cipher :: String -> String -> String
cipher message keyword = encrypt
  where
    mapF char (idx, lst) =
      if char == ' '
        then (idx, ' ' : lst)
        else (mod (idx + 1) (length keyword), (keyword !! idx) : lst)
    zipped = zip message $ reverse $ snd $ foldr mapF (0, []) message
    encrypt =
      map
        (\(x, y) ->
            if y == ' '
              then ' '
              else chr (mod (ord y + ord x - (2 * ord 'A')) 26 + ord 'A'))
        zipped

isSubsequenceOf
  :: (Eq a)
  => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf (x:xs) ys = x `elem` ys && isSubsequenceOf xs ys

capitalizeWords :: String -> [(String, String)]
capitalizeWords sentence = zip parts (map capitalizeWord parts)
  where
    parts = words sentence

capitalizeWord word@(x:xs)
  | x `elem` ['a' .. 'z'] = chr (ord x - ord 'a' + ord 'A') : xs
  | x `elem` ['A' .. 'Z'] = word
