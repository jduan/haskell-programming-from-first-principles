{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Test where

data Price =
  Price Integer
  deriving (Eq, Show)

data Manufacturer
  = Mini
  | Mazda
  | Tata
  deriving (Eq, Show)

data Airline
  = PapuAir
  | CatapultsRUs
  | TakeYourChancesUnited
  deriving (Eq, Show)

data Vehicle
  = Car Manufacturer
        Price
  | Plane Airline
          Integer
  deriving (Eq, Show)

myCar = Car Mini (Price 14000)

urCar = Car Mazda (Price 20000)

clownCar = Car Tata (Price 7000)

doge = Plane PapuAir 20

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane = not . isCar

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m
getManu _ = error "not a car!"

-- nullary
data Example0 =
  Example0
  deriving (Eq, Show)

-- unary
data Example1 =
  Example1 Int
  deriving (Eq, Show)

-- product of Int and String
data Example2 =
  Example2 Int
           String
  deriving (Eq, Show)

data MyType =
  MyVal Int
  deriving (Eq, Show)

-- nullary
data Example =
  MakeExample
  deriving (Show)

-- unary
data Example3 =
  MakeExample3 Int
  deriving (Show)

-- we can derive TooMany because of GeneralizedNewtypeDeriving
newtype Goats =
  Goats Int
  deriving (Eq, Show, TooMany)

type Cows = Int

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42

class TooMany a  where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany (Int, String) where
  tooMany (n, s) = tooMany n

instance TooMany (Int, Int) where
  tooMany (n1, n2) = tooMany (n1 + n2)

data Person = Person
  { name :: String
  , age :: Int
  } deriving (Eq, Show)

-- distributive property
-- data Fiction =
--   Fiction
--   deriving (Show)
--
-- data Nonfiction =
--   Nonfiction
--   deriving (Show)
--
-- data BookType
--   = FictionBook Fiction
--   | NonfictionBook Nonfiction
--   deriving (Show)
type AuthorName = String

-- data Author =
--   Author (AuthorName, BookType)
data Author
  = Fiction AuthorName
  | Nonfiction AuthorName
  deriving (Eq, Show)

data Expr
  = Number Int
  | Add Expr
        Expr
  | Minus Expr
  | Mult Expr
         Expr
  | Divide Expr
           Expr

-- deconstruct values
data GuessWhat =
  Chickenbutt
  deriving (Eq, Show)

data Id a =
  MkId a
  deriving (Eq, Show)

data Product a b =
  Product a
          b
  deriving (Eq, Show)

data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

data RecordProduct a b = RecordProduct
  { pfirst :: a
  , psecond :: b
  } deriving (Eq, Show)

newtype NumCow =
  NumCow Int
  deriving (Eq, Show)

newtype NumPig =
  NumPig Int
  deriving (Eq, Show)

data Farmhouse =
  Farmhouse NumCow
            NumPig
  deriving (Eq, Show)

type Farmhouse' = Product NumCow NumPig

newtype NumSheep =
  NumSheep Int
  deriving (Eq, Show)

data BigFarmhouse =
  BigFarmhouse NumCow
               NumPig
               NumSheep
  deriving (Eq, Show)

type BigFarmhouse' = Product NumCow (Product NumPig NumSheep)

type Name = String

type Age = Int

type LovesMud = Bool

type PoundsOfWool = Int

data CowInfo =
  CowInfo Name
          Age
  deriving (Eq, Show)

data PigInfo =
  PigInfo Name
          Age
          LovesMud
  deriving (Eq, Show)

data SheepInfo =
  SheepInfo Name
            Age
            PoundsOfWool
  deriving (Eq, Show)

data Animal
  = Cow CowInfo
  | Pig PigInfo
  | Sheep SheepInfo
  deriving (Eq, Show)

type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)

type Awesome = Bool

person :: Product Name Awesome
person = Product "Simon" True

data Twitter =
  Twitter
  deriving (Eq, Show)

data AskFm =
  AskFm
  deriving (Eq, Show)

socialNetwork :: Sum Twitter AskFm
socialNetwork = First Twitter
