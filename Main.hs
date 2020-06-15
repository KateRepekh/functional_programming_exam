module Main where

import Data.List

checkCond :: (Integral a) => (a -> a) -> [a] -> Bool
checkCond f [] = False
checkCond f [x] = f x
checkCond f (x:xs)
    | f x = True
    | otherwise = checkCond xs


generateList :: [Integer] -> Integer -> [[Integer]]
generateList xs n
    | length xs == n = xs
    | length xs == 0 = generateList [1] ++ generateList [2] ++ generateList [3]
    | xs !! (length xs - 1) == 3 = generateList (xs ++ [1])
    | xs !! (length xs - 1) == 2 = (generateList (xs ++ [1])) ++ (generateList (xs ++ [2])) 
    | xs !! (length xs - 1) == 1 = (generateList (xs ++ [1])) ++ (generateList (xs ++ [2])) ++ (generateList (xs ++ [3]))


main :: IO ()
main = do
    print generateList [] 3
    print checkCond even [1, 3, 5, 7]
