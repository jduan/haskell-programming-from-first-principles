module Phone where

import Data.Char

convo :: [String]
convo =
  [ "Wanna play 20 questions"
  , "Ya"
  , "U 1st haha"
  , "Lol ok. Have u ever tasted alcohol lol"
  , "Lol ya"
  , "Wow ur cool haha. Ur turn"
  , "Ok. Do u think I am pretty Lol"
  , "Lol ya"
  , "Haha thanks just making sure rofl ur turn"
  ]

data DaPhone
  = End
  | Node Digit
         String
         DaPhone
  deriving (Show)

-- valid buttons = "0123456789*#"
type Digit = Char

-- valid presses: 1 and up
type Presses = Int

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps End _ = []
reverseTaps (Node digit str rest) char
  | char `elem` ['A' .. 'Z'] =
    ('*' :: Digit, 1 :: Presses) :
    reverseTaps (Node digit str rest) (toLower char)
  | char `elem` str = [(digit, findIndex str char)]
  | otherwise = reverseTaps rest char
  where
    findIndex "" char = error "findIndex: char not found"
    findIndex (x:xs) char
      | x == char = 1
      | otherwise = 1 + findIndex xs char

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone [] = []
cellPhonesDead phone (x:xs) = reverseTaps phone x ++ cellPhonesDead phone xs

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps pairs = foldr (\(digit, presses) acc -> acc + presses) 0 pairs

mostPopularLetter phone str = pairs
  where
    presses = map (fingerTaps . reverseTaps phone) str
    pairs = zip str presses

samplePhone :: DaPhone
samplePhone =
  Node '#' ".," $
  Node '0' " " $
  Node '*' "^" $
  Node '9' "wxyz" $
  Node '8' "tuv" $
  Node '7' "pqrs" $
  Node '6' "mno" $
  Node '5' "jkl" $ Node '4' "ghi" $ Node '3' "def" $ Node '2' "abc" End
