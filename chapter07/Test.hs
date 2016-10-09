module Test where

myNum :: Num a => a
myNum = 1

myVal :: Num a => a -> a
myVal f = f + myNum

stillAFunction :: [a] -> [a] -> [a] -> [a]
stillAFunction a b c = a ++ b ++ c


bindExp :: Integer -> String
bindExp x = let y = 5 in
            let z = y + x in
            "the integer was: " ++ show x
            ++ " and y was: " ++ show y
            ++ " and z was: " ++ show z

-- the x in let shadows the x given to the function
-- this is called "lexical/static scoping"
bindExp2 x = let x = 10
                 y = 5 in
                 "the integer was: " ++ show x
                 ++ " and y was: " ++ show y


mTh1 x y z = x * y * z
mTh2 x y = \z -> x * y * z
mTh3 x = \y -> \z -> x * y * z
mTh4 = \x -> \y -> \z -> x * y * z


addOneIfOdd n = case odd n of
                     True -> f n
                     False -> n
    where f = \n -> n + 1

addFive x y = (if x > y then y else x) + 5
addFive2 = \x y -> (if x > y then y else x) + 5

mflip f = \x -> \y -> f y x
mflip2 f x y = f y x

isItTwo 2 = True
isItTwo _ = False


newtype Username = Username String
newtype AccountNumber = AccountNumber Integer

data User = UnregisteredUser
          | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "UnregisteredUser"
-- patch matching
printUser (RegisteredUser (Username name) (AccountNumber accNum)) =
    putStrLn $ name ++ " " ++ show accNum

-- call it
-- printUser (RegisteredUser (Username "jduan") (AccountNumber 112))


data WherePenguinsLive = Galapagos
                       | Antarctica
                       | Australia
                       | SouthAfrica
                       | SouthAmerica
                       deriving (Eq, Show)

data Penguin = Peng WherePenguinsLive
             deriving (Eq, Show)

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica _ = False

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng place) = place

galapagosPenguin :: Penguin -> Bool
galapagosPenguin  (Peng Galapagos) = True
galapagosPenguin  (Peng _) = False
