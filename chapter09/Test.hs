safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead (x:_) = Just x

safetail :: [a] -> Maybe [a]
safetail [] = Nothing
safetail [x] = Nothing
safetail (_:xs) = Just xs

eftBool:: Bool -> Bool -> [Bool]
eftBool x y
    | x == y = [x]
eftBool False True = [False, True]
eftBool True False = []

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd x y
    | x == y = [x]
    | x > y = []
eftOrd LT EQ = [LT, EQ]
eftOrd LT GT = [LT, EQ, GT]
eftOrd EQ GT = [EQ, GT]

eftEnum :: (Ord a, Enum a) => a -> a -> [a]
eftEnum x y
    | x > y = []
    | x == y = [x]
    | otherwise = reverse $ go x y []
    where go x y acc
              | x > y = acc
              | otherwise = go (succ x) y (x:acc)

eftInt :: Int -> Int -> [Int]
eftInt = eftEnum

eftChar :: Char -> Char -> String
eftChar = eftEnum

-- split a sentence into words separated by spaces
myWords :: String -> [String]
myWords sentence = splitBy sentence ' '

splitBy :: String -> Char -> [String]
splitBy sentence delimiter = go sentence []
    where go "" acc = reverse acc
          go str acc = go (dropFirstWord str) (getFirstWord str : acc)
          isDelimiter ch = ch == delimiter
          isNotDelimiter ch = not $ isDelimiter ch
          getFirstWord = takeWhile isNotDelimiter
          dropFirstWord str = dropWhile isDelimiter $ dropWhile isNotDelimiter str


-- remove all articles (the, a, an) from a sentence
removeArticles :: String -> [String]
removeArticles sentence = filter (not . isArticle) $ words sentence
    where isArticle word = word `elem` ["the", "a", "an"]


myzip :: [a] -> [b] -> [(a, b)]
myzip [] _ = []
myzip _ [] = []
myzip (x:xs) (y:ys) = (x, y) : myzip xs ys

myzipwith :: (a -> b -> c) -> [a] -> [b] -> [c]
myzipwith f [] _ = []
myzipwith f _ [] = []
myzipwith f (x:xs) (y:ys) = f x y : myzipwith f xs ys

myzip2 = myzipwith (\x y -> (x, y))
