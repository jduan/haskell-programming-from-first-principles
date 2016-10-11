module Cipher where

import Data.Char

-- a basic Caesar cipher that shifts rightward
caesar :: Int -> String -> String
caesar spaces = map shift
    where normal = spaces `mod` 26
          base = ord 'a' - 1
          shift ch
              | ch `elem` ['a'..'z'] = shiftLower ch
              | ch `elem` ['A'..'Z'] = toUpper $ shiftLower $ toLower ch
              | otherwise = error "not in alphabet!"
          shiftLower ch = if newOrd > ord 'z' then chr (base + newOrd - ord 'z')
                                              else chr newOrd
            where newOrd = ord ch + normal


unCaesar :: Int -> String -> String
unCaesar spaces = caesar $ 26 - (spaces `mod` 26)
