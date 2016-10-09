module PatternMatching where

addEmUp2 :: Num a => (a, a) -> a
addEmUp2 (x, y) = x + y

addEmUp2Alt :: Num a => (a, a) -> a
addEmUp2Alt tup = fst tup + snd tup

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

third3 :: (a, b, c) -> c
third3 (_, _, x) = x

k (x, y) = x
k1 = k ((4 - 1), 10)
k2 = k ("three", (1 + 2))
k3 = k (3, True)

f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, b, c) (d, e, f) = ((a, d), (c, f))


functionC x y = case x > y of
                     True -> x
                     False -> y

ifEvenAdd2 n = case even n of
                    True -> n + 2
                    False -> n

nums x = case compare x 0 of
              LT -> -1
              GT -> 1
              EQ -> 0

