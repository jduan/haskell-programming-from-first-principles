module StringOps where

append_bang :: String -> String
append_bang s = s ++ "!"

get_5th_char :: String -> String
get_5th_char s = take 1 (drop 4 s)

drop_9 :: String -> String
drop_9 s = drop 9 s

thirdLetter :: String -> Char
thirdLetter s = s !! 2

letterIndex :: Int -> Char
letterIndex i = "Curry is awesome!" !! i

-- reverse a sentence by words
rvrs :: String -> String
rvrs s = let cu = take 5 s
             is = take 2 (drop 6 s)
             aw = drop 9 s
         in aw ++  " " ++ is ++ " " ++ cu
