module Test where

addStuff :: Integer -> Integer -> Integer
addStuff a b = a + b + 5

subtractStuff :: Integer -> Integer -> Integer
subtractStuff x y = x - y - 10

nonsense :: Num t => Bool -> t
nonsense True = 805
nonsense False = 31337

-- The following functions are the "same"
curriedFunction :: Integer -> Bool -> Integer
curriedFunction i b = i + nonsense b

uncurriedFunction :: (Integer, Bool) -> Integer
uncurriedFunction (i, b) = i + nonsense b

anonymouse :: Integer -> Bool -> Integer
anonymouse = \i b -> i + nonsense b

anonNested :: Integer -> Bool -> Integer
anonNested = \i -> \b -> i + nonsense b

id2 :: a -> a -> a
id2 x y = x

id3 :: a -> a -> a
id3 x y = y

id4 :: a -> b -> b
id4 x y = y

triple :: Integer -> Integer
triple x = x * 3

triple2 x = tripleItYo x
    where tripleItYo :: Integer -> Integer
          tripleItYo y = y * 3
