module TypeKwonDo where

data Woot

data Blah

f :: Woot -> Blah
f = undefined

g :: (Blah, Woot) -> (Blah, Blah)
g (x, y) = (x, f y)

-------------

f2 :: Int -> String
f2 = undefined

g2 :: String -> Char
g2 = undefined

h :: Int -> Char
h x = g2 $ f2 x

-------------

data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e x = w $ q x

-------------

data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (x, y) = (xz x, yz y)

-------------

munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge xToY yToWZ x = fst $ yToWZ $ xToY x

-------------

