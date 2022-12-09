module P6b (run) where

import Data.List (findIndex, nub)

markerLength :: Int
markerLength = 14

run :: String -> Maybe Int
run = findMarker . slidingWindow markerLength

slidingWindow :: Int -> [a] -> [[a]]
slidingWindow count xs
  | length xs < count = []
  | otherwise = take count xs : slidingWindow count (tail xs)

findMarker :: [String] -> Maybe Int
findMarker = fmap (+markerLength) . findIndex noRepeats

noRepeats :: String -> Bool
noRepeats xs = length (nub xs) == length xs
