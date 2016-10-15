module Palindrome where

import Control.Monad
import Data.Char (toLower)
import System.Exit (exitSuccess)

isPalinedrome :: String -> Bool
isPalinedrome xs = xs' == reverse xs'
    where xs' = filter (\x -> x `elem` ['a'..'z']) lowered
          lowered = map toLower xs

palinedrome :: IO ()
palinedrome = forever $ do
  line <- getLine
  case (isPalinedrome line) of
       True -> putStrLn "It's a palinedrome!"
       False -> do
         putStrLn "Nope!"
         exitSuccess
