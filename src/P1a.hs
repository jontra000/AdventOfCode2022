module P1a (run1, run2, inputLocation) where

import Data.List.Split (splitOn)

run1 :: String -> Int
run1 = solve . parse

run2 :: String -> Int
run2 _ = 0

inputLocation :: String
inputLocation = "inputs/input1"

parse :: String -> [[Int]]
parse = map parseElf . splitOn [""] . lines

parseElf :: [String] -> [Int]
parseElf = map read

solve :: [[Int]] -> Int
solve = maximum . map sum