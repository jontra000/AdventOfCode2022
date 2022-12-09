module P6a (run) where

import Data.List (findIndex, nub)

run :: String -> Maybe Int
run = findMarker . slidingWindow

slidingWindow :: [a] -> [[a]]
slidingWindow xs@(a:b:c:d:_) = [a,b,c,d] : slidingWindow (tail xs)
slidingWindow _ = []

findMarker :: [String] -> Maybe Int
findMarker = fmap (+4) . findIndex noRepeats

noRepeats :: String -> Bool
noRepeats xs = length (nub xs) == length xs
