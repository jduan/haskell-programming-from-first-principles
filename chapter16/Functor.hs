data Maybe1 a
  = Nothing1
  | Just1 a
  deriving (Show, Eq)

instance Functor Maybe1 where
  fmap f Nothing1 = Nothing1
  fmap f (Just1 a) = Just1 (f a)

data FixMePls a
  = FixMe
  | Pls a
  deriving (Show, Eq)

instance Functor FixMePls where
  fmap f FixMe = FixMe
  fmap f (Pls a) = Pls (f a)

data WhoCares a
  = ItDoesnt
  | Matter a
  | WhatThisIsCalled
  deriving (Show, Eq)

instance Functor WhoCares where
  fmap f ItDoesnt = ItDoesnt
  fmap f (Matter a) = Matter (f a)
  fmap f WhatThisIsCalled = WhatThisIsCalled

data CountingBad a =
  Heisenberg Int
             a
  deriving (Show, Eq)

instance Functor CountingBad where
  fmap f (Heisenberg i a) = Heisenberg i (f a)
