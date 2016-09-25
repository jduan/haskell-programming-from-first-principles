module Learn where

sayHello :: String -> IO ()
sayHello x = putStrLn ("hello, " ++ x ++ "!")

triple :: (Num a) => a -> a
triple x = x * 3

half :: (Fractional a) => a -> a
half x = x / 2

square :: (Num a) => a -> a
square x = x * x

area :: (Floating a) => a -> a
area r = pi * (r * r)

perimeter x y = (x * 2) + (y * 2)
perimeter2 x y = x * 2 + y * 2

f x = x / 2 + 9
f2 x = x / (2 + 9)

x = 10 * 5 + y
myResult = x * 5
y = 10

x2 = 7
y2 = 10
f3 = x2 + y2


-- let vs where
printInc n = print plusTwo
    where plusTwo = n + 2

printInc2 n = let plusTwo = n + 2
                  in print plusTwo

mult1 = x * y
    where x = 5
          y = 6

mult2 = let x = 5
            y = 6
        in x * y

xy1 = let x = 3
          y = 1000
      in x * 3 + y

xy2 = let y = 10
          x = 10 * 5 + y
      in x * 5

xy3 = let x = 7
          y = negate x
          z = y * 10
      in z / x + y

bool1 = 2 + 2 * 3 - 1 == 2 + (2 * 3) - 1
bool2 = ((^) 10 $ 1 + 1) == 10 ^ (1 + 1)
bool3 = 2 ^ 2 * 4 ^ 5 + 1 == (2 ^ 2) * (4 ^ 5) + 1

waxOn = let z = 7
            y = z + 8
            x = y ^ 2
        in x * 5

waxOff x = triple x

n = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]
