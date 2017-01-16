module Identify where

newtype Identify a =
  Identify a
  deriving (Show, Eq)

instance Functor Identify where
  fmap f (Identify a) = Identify (f a)

instance Applicative Identify where
  pure = Identify
  Identify f <*> Identify a = Identify (f a)
