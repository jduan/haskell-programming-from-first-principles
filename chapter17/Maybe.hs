module Maybe where

validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
  if length s > maxLen
    then Nothing
    else Just s

newtype Name =
  Name String
  deriving (Show, Eq)

newtype Address =
  Address String
  deriving (Show, Eq)

data Sex
  = Male
  | Female
  deriving (Show, Eq)

mkName :: String -> Maybe Name
mkName s = Name <$> validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a = Address <$> validateLength 100 a

mkSex :: String -> Maybe Sex
mkSex "m" = Just Male
mkSex "f" = Just Female
mkSex _ = Nothing

data Person =
  Person Name
         Address
         Sex
  deriving (Show, Eq)

mkPerson :: String -> String -> String -> Maybe Person
mkPerson name address sex =
  case mkName name of
    Nothing -> Nothing
    Just name' ->
      case mkAddress address of
        Nothing -> Nothing
        Just address' ->
          case mkSex sex of
            Nothing -> Nothing
            Just sex' -> Just $ Person name' address' sex'

mkPerson' :: String -> String -> String -> Maybe Person
mkPerson' name address sex =
  Person <$> mkName name <*> mkAddress address <*> mkSex sex

data Cow = Cow
  { name :: String
  , age :: Int
  , weight :: Int
  } deriving (Show, Eq)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n
  | n >= 0 = Just n
  | otherwise = Nothing

cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString name age weight =
  Cow <$> noEmpty name <*> noNegative age <*> noNegative weight
-- or
  -- liftA3 Cow (noEmpty name) (noNegative age) (noNegative weight)
