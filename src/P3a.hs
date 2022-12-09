module P3a (run) where

import Data.List (intersect)
import Data.Char (ord)

run :: String -> Int
run = solve . parse

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