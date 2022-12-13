module P3b (run1, run2, inputLocation) where

import Data.List (intersect)
import Data.Char (ord)

run1 :: String -> Int
run1 = solve . parse

run2 :: String -> Int
run2 _ = 0

inputLocation :: String
inputLocation = "inputs/input3"

parse :: String -> [[String]]
parse = group3 . lines

group3 :: [String] -> [[String]]
group3 (a:b:c:xs) = [a,b,c] : group3 xs
group3 [] = []
group3 xs = [xs]

solve :: [[String]] -> Int
solve = sum . map solveGroup

solveGroup :: [String] -> Int
solveGroup = priority . findRepeatedItem

findRepeatedItem :: [String] -> Char
findRepeatedItem = head . foldl1 intersect

priority :: Char -> Int
priority = asciiToPriority . ord

asciiToPriority :: Int -> Int
asciiToPriority a = if a < 97 then a - 38 else a - 96