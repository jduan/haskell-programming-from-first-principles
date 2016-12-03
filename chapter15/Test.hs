module Test where

vowels :: String -> Int
vowels = length . filter (`elem` "aeiou")
