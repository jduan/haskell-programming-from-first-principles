module Constant where

newtype Constant a b = Constant
  { getConstant :: a
  } deriving (Show, Eq)

instance Functor (Constant a) where
  fmap f (Constant a) = Constant a

instance Monoid a =>
         Applicative (Constant a) where
  pure _ = Constant mempty
  Constant c1 <*> Constant c2 = Constant (c1 `mappend` c2)
