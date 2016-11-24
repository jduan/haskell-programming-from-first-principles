module Test where

ifEvenAdd2 :: Integer -> Maybe Integer
ifEvenAdd2 n =
  if even n
    then Just (n + 2)
    else Nothing

type Name = String

type Age = Integer

data Person =
  Person Name
         Age
  deriving (Show)

data PersonInvalid
  = NameEmtpy
  | AgeTooLow
  deriving (Eq, Show)

type ValidatePerson a = Either [PersonInvalid] a

ageOkay :: Age -> ValidatePerson Age
ageOkay age =
  case age >= 0 of
    True -> Right age
    False -> Left [AgeTooLow]

nameOkay :: Name -> ValidatePerson Name
nameOkay name =
  case name /= "" of
    True -> Right name
    False -> Left [NameEmtpy]

mkPerson :: Name -> Age -> ValidatePerson Person
mkPerson name age = mkPerson' (nameOkay name) (ageOkay age)

mkPerson' :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
mkPerson' (Right name) (Right age) = Right (Person name age)
mkPerson' (Left badName) (Left badAge) = Left (badName ++ badAge)
mkPerson' (Left badName) _ = Left badName
mkPerson' _ (Left badAge) = Left badAge


-- kinds
data Example a = Blah | RoofGoats | Woot a
