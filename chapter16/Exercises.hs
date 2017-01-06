{-# LANGUAGE FlexibleInstances #-}

module Exercises where

-- 3
newtype Flip f a b =
  Flip (f b a)
  deriving (Show, Eq)

newtype K a b =
  K a
  deriving (Show, Eq)

instance Functor (Flip K a) where
  fmap f (Flip (K b)) = Flip (K (f b))

-- 4
data EvilGoateeConst a b =
  GoatyConst b
  deriving (Show, Eq)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst $ f b

-- 5
data LiftItOut f a =
  LiftItOut (f a)
  deriving (Show, Eq)

instance Functor f =>
         Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

-- 6
data Parappa f g a =
  DaWrappa (f a)
           (g a)
  deriving (Show, Eq)

instance (Functor f, Functor g) =>
         Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

-- 7
data IgnoreOne f g a b =
  IgnoringSomething (f a)
                    (g b)
  deriving (Show, Eq)

instance (Functor f, Functor g) =>
         Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)

-- 8
data Notorious g o a t =
  Notorious (g o)
            (g a)
            (g t)
  deriving (Show, Eq)

instance Functor g =>
         Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

-- 9
data List a
  = Nil
  | Cons a
         (List a)
  deriving (Show, Eq)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a lst) = Cons (f a) (fmap f lst)

-- 10
data GoatLord a
  = NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a)
              (GoatLord a)
              (GoatLord a)
  deriving (Show, Eq)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats ga1 ga2 ga3) =
    MoreGoats (fmap f ga1) (fmap f ga2) (fmap f ga3)

-- 11
data TalkToMe a
  = Halt
  | Print String
          a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read sa) = Read (f . sa)
