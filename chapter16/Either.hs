incIfRight
  :: Num a
  => Either e a -> Either e a
incIfRight (Left e) = Left e
incIfRight (Right a) = Right (a + 1)

showIfRight
  :: Show a
  => Either e a -> Either e String
showIfRight (Left e) = Left e
showIfRight (Right a) = Right $ show a

incEither
  :: Num a
  => Either e a -> Either e a
incEither = fmap (+ 1)

showEither
  :: Show a
  => Either e a -> Either e String
showEither = fmap show

liftedInc
  :: (Functor f, Num b)
  => f b -> f b
liftedInc = fmap (+ 1)

liftedShow
  :: (Functor f, Show b)
  => f b -> f String
liftedShow = fmap show

data Sum a b
  = First a
  | Second b
  deriving (Show, Eq)

instance Functor (Sum a) where
  fmap f (First a) = First a
  fmap f (Second b) = Second (f b)
