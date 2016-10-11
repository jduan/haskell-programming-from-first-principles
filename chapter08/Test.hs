module Test where

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

applyTimes 0 f base = base
applyTimes times f base = f (applyTimes (times - 1) f base)

incTimes times base = applyTimes times (+ 1) base

f :: Bool -> Maybe Int
f False = Just 0
f _ = Nothing

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)


-- type aliases
type Numerator = Integer
type Denominator = Integer
type Quotient = Integer
type Remainder = Integer

data DividedResult = Result Integer Integer
                   | DividedByZero
                   deriving Show

dividedBy :: Numerator -> Denominator -> DividedResult
dividedBy numerator denominator -- = go numerator denominator 0
    | denominator == 0 = DividedByZero
    | numerator == 0 = Result 0 0
    | numerator < 0 && denominator < 0 = go (negate numerator) (negate denominator) 0
    | numerator > 0 && denominator > 0 = go numerator denominator 0
    | numerator > 0 && denominator < 0 = negateResult $ go numerator (negate denominator) 0
    | numerator < 0 && denominator > 0 = negateResult $ go (negate numerator) denominator 0
    where
          go :: Integer -> Integer -> Integer -> DividedResult
          go numer denom times
              | numer < denom = Result times numer
              | otherwise = go (numer - denom) denom (times + 1)
          negateResult :: DividedResult -> DividedResult
          negateResult (Result x y) = Result (-x) y

dividedBy2 :: Numerator -> Denominator -> (Quotient, Remainder)
dividedBy2 numerator denominator = go numerator 0
-- in this version, we use the "denominator" from the closure
    where go numer times
              | numer < denominator = (times, numer)
              | otherwise = go (numer - denominator) (times + 1)
