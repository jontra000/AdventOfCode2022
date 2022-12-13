module P1b (run1, run2, inputLocation) where

import Data.List.Split (splitOn)
import Data.List (sort)

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
solve = sum . take 3 . reverse . sort . map sum