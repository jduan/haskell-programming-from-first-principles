module IOFunctor where

getInt :: IO Int
getInt = fmap read getLine

meTooIsm :: IO String
meTooIsm = do
  input <- getLine
  return (input ++ " and me too!")

bumpIt :: IO Int
bumpIt = do
  intVal <- getInt
  return (intVal + 1)
