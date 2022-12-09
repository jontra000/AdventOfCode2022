module P1a (run) where

import Data.List.Split (splitOn)

run :: String -> Int
run = solve . parse

parse :: String -> [[Int]]
parse = map parseElf . splitOn [""] . lines

parseElf :: [String] -> [Int]
parseElf = map read

solve :: [[Int]] -> Int
solve = maximum . map sum