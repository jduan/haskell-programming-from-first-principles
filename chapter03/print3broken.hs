module Print3Broken where

greeting = "Yarrrrr"

printSecond :: IO ()
printSecond = putStrLn greeting

main :: IO ()
main = do
    putStrLn greeting
    printSecond
