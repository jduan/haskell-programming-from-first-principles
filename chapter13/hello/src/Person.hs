module Person where

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty
                    | AgeTooLow
                    | PersonInvalidUnknown String
                   deriving (Eq, Show)

mkPerson :: Name
         -> Age
         -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | age <= 0 = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $
                       "Name was: " ++ show name ++
                       " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStrLn "Enter your name: "
  name <- getLine
  putStrLn "Enter your age: "
  age <- getLine
  case mkPerson name (read age :: Integer) of
       Left NameEmpty -> putStrLn "Name can't be empty!"
       Left AgeTooLow -> putStrLn "Age has to be positive!"
       Left (PersonInvalidUnknown reason) -> putStrLn reason
       Right person -> putStrLn $ "Yah! Successfully got a person: " ++ show person
