module PoemLines where

firstSen = "Tyger Tyger, burning bright\n\n\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen
    ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines sentence = splitBy sentence '\n'

splitBy :: String -> Char -> [String]
splitBy sentence delimiter = go sentence []
    where go "" acc = reverse acc
          go str acc = go (dropFirstWord str) (getFirstWord str : acc)
          isDelimiter ch = ch == delimiter
          isNotDelimiter ch = not $ isDelimiter ch
          getFirstWord = takeWhile isNotDelimiter
          dropFirstWord str = dropWhile isDelimiter $ dropWhile isNotDelimiter str

shouldEqual =
    [ "Tyger Tyger, burning bright"
    , "In the forests of the night"
    , "What immortal hand or eye"
    , "Could frame thy fearful symmetry?"
    ]

main :: IO ()
main =
    print $ "Are they equal? " ++ show (myLines sentences == shouldEqual)
