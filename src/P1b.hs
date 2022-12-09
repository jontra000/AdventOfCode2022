module P1b (run) where

import Data.List.Split (splitOn)
import Data.List (sort)

run :: String -> Int
run = solve . parse

parse :: String -> [[Int]]
parse = map parseElf . splitOn [""] . lines

parseElf :: [String] -> [Int]
parseElf = map read

solve :: [[Int]] -> Int
solve = sum . take 3 . reverse . sort . map sum