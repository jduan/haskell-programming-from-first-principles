module Exercises where

x :: Int -> Int
x blah = blah + 20

-- This code won't typecheck!
-- printIt :: IO ()
-- printIt = putStrLn (show x)

data Person = Person Bool
            deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

printPerson2 :: Person -> IO ()
printPerson2 (Person True) = print True
printPerson2 (Person False) = print False


data Mood = Blah
          | Woot
          deriving (Eq, Ord, Show)

settleDown x = if x == Woot
                  then Blah
                  else x

-- type aliases
type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object
              deriving (Eq, Show)

s1 = Sentence "dogs" "drool" "saliva"
s2 = Sentence "Julie" "loves" "dogs"


data Rocks = Rocks String deriving (Eq, Ord, Show)

data Yeah = Yeah Bool deriving (Eq, Ord, Show)

data Papu = Papu Rocks Yeah deriving (Eq, Ord, Show)

-- doesn't typecheck
-- phew = Papu "chases" True

truth = Papu (Rocks "chomskydoz") (Yeah True)

equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'
