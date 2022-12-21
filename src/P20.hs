module P20 (run1, run2, inputLocation) where

import Data.List (foldl', findIndex)
import Data.Tuple (swap)

inputLocation :: String
inputLocation = "inputs/input20"

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2  = solve2 . parse

parse :: String -> [Int]
parse = map read . lines

solve1 :: [Int] -> Int
solve1 = sum . groveCoordinates . mix

solve2 :: [Int] -> Int
solve2 = sum . groveCoordinates . map fst . (!! 10) . iterate mixTagged . map swap . zip [0..] . map (* 811589153)

mix :: [Int] -> [Int]
mix xs = map fst $ mixTagged (zip xs [0..])

mixTagged :: [(Int, Int)] -> [(Int, Int)]
mixTagged xs = foldl' mix' xs [0..length xs - 1]

mix' :: [(Int, Int)] -> Int -> [(Int, Int)]
mix' xs i =
    case findIndex ((== i) . snd) xs of
        Nothing -> error ("bad list")
        Just currentI ->
            let (prev,x:next) = splitAt currentI xs
                i' = circularIndex (length xs) (fst x + currentI)
            in  if i' > currentI
                then
                    let (mid,end) = splitAt (i' - currentI) next
                    in  prev ++ mid ++ [x] ++ end
                else
                    let (start,mid) = splitAt i' prev
                    in  start ++ [x] ++ mid ++ next

circularIndex :: Int -> Int -> Int
circularIndex 0 _ = error "div by 0"
circularIndex period i
    | i < 0 = i `mod` (period - 1)
    | otherwise = i `mod` (period - 1)

baseToZero :: [Int] -> [Int]
baseToZero xs =
    let (toWrap,res) = break (==0) xs
    in  res ++ toWrap

groveCoordinates :: [Int] -> [Int]
groveCoordinates xs = map (baseToZero xs !!) [1000 `mod` period, 2000 `mod` period, 3000 `mod` period]
    where period = length xs