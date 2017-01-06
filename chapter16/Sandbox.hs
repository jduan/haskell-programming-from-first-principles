module Sandbox where

-- this doesn't work!
-- e :: IO Integer
-- e =
--   let ioi = readIO "1" :: IO Integer
--       changed = read ("123" ++) show ioi
--   in (* 3) changed
data Two a b =
  Two a
      b
  deriving (Show, Eq)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

data Or a b
  = First a
  | Second b
  deriving (Show, Eq)

instance Functor (Or a) where
  fmap f (First a) = First a
  fmap f (Second b) = Second (f b)

newtype Identity a =
  Identity a
  deriving (Show, Eq)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

data Pair a =
  Pair a
       a
  deriving (Show, Eq)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

data Three a b c =
  Three a
        b
        c
  deriving (Show, Eq)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

data Three' a b =
  Three' a
         b
         b
  deriving (Show, Eq)

instance Functor (Three' a) where
  fmap f (Three' a b1 b2) = Three' a (f b1) (f b2)

data Four a b c d =
  Four a
       b
       c
       d
  deriving (Show, Eq)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

data Four' a b =
  Four' a
        a
        a
        b
  deriving (Show, Eq)

instance Functor (Four' a) where
  fmap f (Four' a1 a2 a3 b) = Four' a1 a2 a3 (f b)
-- data Wrap f a =
--   Wrap (f a)
--   deriving (Show, Eq)
--
-- instance Functor (Wrap f) where
--   fmap f2 (Wrap fa) = Wrap (f2 fa)
