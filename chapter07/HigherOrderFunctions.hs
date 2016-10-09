module HigherOrderFunctions where

data Employee = Coder
              | Manager
              | Veep
              | CEO
              deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss boss employee =
    putStrLn $ show boss ++ " is the boss of " ++ show employee

employeeRank :: Employee -> Employee -> IO ()
employeeRank e e' =
    case compare e e' of
         LT -> (flip reportBoss) e e'
         GT -> reportBoss e e'
         EQ -> putStrLn "Neither employee is the boss"


employeeRank2 :: (Employee -> Employee -> Ordering)
                -> Employee
                -> Employee
                -> IO ()
employeeRank2 f e e' =
    case f e e' of
         LT -> (flip reportBoss) e e'
         GT -> reportBoss e e'
         EQ -> putStrLn "Neither employee is the boss"

codersRuleCEOsDrool:: Employee -> Employee -> Ordering
codersRuleCEOsDrool Coder Coder = EQ
codersRuleCEOsDrool Coder _ = GT
codersRuleCEOsDrool _ Coder = LT
codersRuleCEOsDrool e e' = compare e e'


dodgy x y = x + y * 10
oneIsOne = dodgy 1
oneIsTwo = (flip dodgy) 2
