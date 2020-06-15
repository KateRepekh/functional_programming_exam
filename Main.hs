module Main where

import Data.List

checkCond :: (Integral a) => (a -> Bool) -> [a] -> Bool
checkCond f [] = False
checkCond f [x] = f x
checkCond f (x:xs)
    | f x = True
    | otherwise = (checkCond f xs)


generateList :: [Int] -> Int -> [[Int]]
generateList xs n
    | (length xs) == n = [xs]
    | (length xs) == 0 = (generateList [1] n) ++ (generateList [2] n) ++ (generateList [3] n)
    | xs !! (length xs - 1) == 3 = (generateList (xs ++ [1]) n)
    | xs !! (length xs - 1) == 2 = (generateList (xs ++ [1]) n) ++ (generateList (xs ++ [2]) n) 
    | xs !! (length xs - 1) == 1 = (generateList (xs ++ [1]) n) ++ (generateList (xs ++ [2]) n) ++ (generateList (xs ++ [3]) n)

main = do
    print (generateList [] 3)
    print (checkCond even [1, 3, 5, 7])
