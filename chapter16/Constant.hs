module Constant where

newtype Constant a b = Constant
  { getConstant :: a
  } deriving (Show, Eq)

instance Functor (Constant a) where
  fmap f (Constant v) = Constant v
