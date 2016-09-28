module GreetIfCool1 where

greetIfCool :: String -> IO ()
greetIfCool coolness =
    if cool coolness
       then putStrLn "eyyyyy. What's shaking?"
    else
        putStrLn "shhh."
    where cool v = v == "downright frosty yo"

greetIfCool2 :: String -> IO ()
greetIfCool2 coolness =
    let cool v = v == "downright frosty yo"
    in
        if cool coolness
           then putStrLn "eyyyyy. What's shaking?"
        else
            putStrLn "shhh."
