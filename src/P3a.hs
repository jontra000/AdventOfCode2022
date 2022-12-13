module P3a (run1, run2, inputLocation) where

import Data.List (intersect)
import Data.Char (ord)

run1 :: String -> Int
run1 = solve . parse

run2 :: String -> Int
run2 _ = 0

inputLocation :: String
inputLocation = "inputs/input3"

parse :: String -> [String]
parse = lines

solve :: [String] -> Int
solve = sum . map solveLine

solveLine :: String -> Int
solveLine = priority . findRepeatedItem

findRepeatedItem :: String -> Char
findRepeatedItem = commonItem . divideCompartments

divideCompartments :: String -> (String, String)
divideCompartments s = splitAt (length s `div` 2) s

commonItem :: (String, String) -> Char
commonItem (a, b) = head $ intersect a b

priority :: Char -> Int
priority = asciiToPriority . ord

asciiToPriority :: Int -> Int
asciiToPriority a = if a < 97 then a - 38 else a - 96