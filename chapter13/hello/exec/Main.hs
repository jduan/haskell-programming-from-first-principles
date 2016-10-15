module Main where

import Hello
import DogsRule
import System.IO

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

newtype WordList =
  WordList [String]
  deriving (Eq, Show)

allWords :: IO WordList
allWords = do
  dict <- readFile "data/words"
  return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

numGuessesAllowed :: Int
numGuessesAllowed = 10

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where gameLength w =
          let l = length (w :: String)
              in l > minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle word = Puzzle word (map (const Nothing) word) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) char = char `elem` word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) char = char `elem` guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar guessed) guessChar =
  Puzzle word newFilledInSoFar guessed
  where zipper wordChar fillChar =
          if wordChar == guessChar
             then Just wordChar
             else fillChar
        newFilledInSoFar =
          zipWith zipper word filledInSoFar


incorrectGuess :: Puzzle -> Char -> Puzzle
incorrectGuess (Puzzle word filledInSoFar guessed) char =
  Puzzle word filledInSoFar (char : guessed)


handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
       (_, True) -> do
         putStrLn "You already guessed that character, pick something else!"
         return puzzle
       (True, _) -> do
         putStrLn "This character was in the word, filling the word accordingly"
         return (fillInCharacter puzzle guess)
       (False, _) -> do
         putStrLn "This character wasn't in the word, try again."
         return (incorrectGuess puzzle guess)


gameOver :: Puzzle -> IO ()
gameOver (Puzzle word _ guessed) =
  if length guessed > numGuessesAllowed then
    do putStrLn "You lose!"
       putStrLn $ "The word was: " ++ word
       exitSuccess
  else
    return ()


gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar then
    do putStrLn "You win!"
       exitSuccess
  else
    return ()


runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameWin puzzle
  gameOver puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
       [c] -> handleGuess puzzle c >>= runGame
       _   -> putStrLn "Your guess must be a single character"


main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle


-- main :: IO ()
-- main = do
--   hSetBuffering stdout NoBuffering
--   putStr "Please input your name: "
--   name <- getLine
--   sayHello name
--   dogs
--
-- main2 :: IO ()
-- main2 = do c <- getChar
--            c' <- getChar
--            if c == c'
--               then putStrLn "True"
--               else return ()
