module ListComprehensions where

mysqr = [x^2 | x <- [1..5]]
mycube = [x^3 | x <- [1..5]]

mkTuples xs ys = [(x, y) | x <- xs, y <- ys]

mkTuples2 xs ys = [(x, y) | x <- xs, y <- ys, x < 50, y < 50]
