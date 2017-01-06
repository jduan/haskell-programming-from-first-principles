{-# LANGUAGE FlexibleInstances #-}
module FlipFunctor where

data Tuple a b =
  Tuple a
        b
  deriving (Show, Eq)

newtype Flip f a b =
  Flip (f b a)
  deriving (Show, Eq)

instance Functor (Flip Tuple a) where
  fmap f (Flip (Tuple a b)) = Flip (Tuple (f a) b)
