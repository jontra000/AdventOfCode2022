module P6a (run1, run2, inputLocation) where

import Data.List (findIndex, nub)

run1 :: String -> Maybe Int
run1 = findMarker . slidingWindow

run2 :: String -> String
run2 _ = ""

inputLocation :: String
inputLocation = "inputs/input6"

slidingWindow :: [a] -> [[a]]
slidingWindow xs@(a:b:c:d:_) = [a,b,c,d] : slidingWindow (tail xs)
slidingWindow _ = []

findMarker :: [String] -> Maybe Int
findMarker = fmap (+4) . findIndex noRepeats

noRepeats :: String -> Bool
noRepeats xs = length (nub xs) == length xs
