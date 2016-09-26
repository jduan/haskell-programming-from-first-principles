module Reverse where

rvrs :: String -> String
rvrs s = let cu = take 5 s
             is = take 2 (drop 6 s)
             aw = drop 9 s
         in aw ++  " " ++ is ++ " " ++ cu

main :: IO ()
main = print $ rvrs "Curry is awesome"
