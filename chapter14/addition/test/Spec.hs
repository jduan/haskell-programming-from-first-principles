module Main where

import Lib
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
    describe "Addition" $ do
        it "1 + 1 is greater than 1" $ do
            (1 + 1) > 1 `shouldBe` True
        it "mySum should add two numbers" $ do
            mySum 1 2 `shouldBe` 3
        it "15 divided by 3 is 5" $ do
            dividedBy 15 3 `shouldBe` (5, 0)
        it "22 divided by 5 is 4 remainder 2" $ do
            dividedBy 22 5 `shouldBe` (4, 2)
        it "3 times 5 is 15" $ do
            multiply 3 5 `shouldBe` 15
        it "3 times -5 is -15" $ do
            multiply 3 (-5) `shouldBe` (-15)

        it "x + 1 is always greater than x" $ do
            property $ \x -> x + 1 > (x :: Int)
