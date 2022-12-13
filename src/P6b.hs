module P6b (run1, run2, inputLocation) where

import Data.List (findIndex, nub)

markerLength :: Int
markerLength = 14

run1 :: String -> Maybe Int
run1 = findMarker . slidingWindow markerLength

run2 :: String -> String
run2 _ = ""

inputLocation :: String
inputLocation = "inputs/input6"

slidingWindow :: Int -> [a] -> [[a]]
slidingWindow count xs
  | length xs < count = []
  | otherwise = take count xs : slidingWindow count (tail xs)

findMarker :: [String] -> Maybe Int
findMarker = fmap (+markerLength) . findIndex noRepeats

noRepeats :: String -> Bool
noRepeats xs = length (nub xs) == length xs
