module ListApplicative where

data List a
  = Nil
  | Cons a
         (List a)
  deriving (Show, Eq)

instance Applicative List where
  pure = Nil
  Nil <*> lst2 = Nil
  lst1 <*> Nil = Nil
  (Cons x xs) <*> lst2 = concat (x <$> lst2) (xs <*> lst2)
    where
      concat Nil l2 = l2
      concat (Cons y ys) l2 = Cons y (concat ys l2)
