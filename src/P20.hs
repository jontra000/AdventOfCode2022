module P20 (run1, run2, inputLocation) where

import Data.List (foldl')
import Data.Foldable (toList)
import qualified Data.Sequence as S

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
solve2 = sum . groveCoordinates . mixN 10 . applyDecryptionKey

applyDecryptionKey :: [Int] -> [Int]
applyDecryptionKey = map (* 811589153)

mix :: [Int] -> [Int]
mix = mixN 1

mixN :: Int -> [Int] -> [Int]
mixN n xs = positionsToValues xs $ (!! n) $ tagStream xs

mixTagged :: [(Int, Int)] -> S.Seq Int -> S.Seq Int
mixTagged values positions = foldl' mix' positions values

tagStream :: [Int] -> [S.Seq Int]
tagStream xs = iterate (mixTagged indexedValues) ixs
    where ixs = S.fromList [0..length xs - 1]
          indexedValues = zip [0..] xs

mix' :: S.Seq Int -> (Int, Int) -> S.Seq Int
mix' xs (i, x) =
    let location = seqElemIndex xs i
        location' = circularIndex (length xs) (location + x)
    in  shift i location location' xs

seqElemIndex :: S.Seq Int -> Int -> Int
seqElemIndex xs i = case S.elemIndexL i xs of
    Just x -> x
    Nothing -> error "Element not found"

shift :: Int -> Int -> Int -> S.Seq Int -> S.Seq Int
shift value start end xs =
    let xs' = S.deleteAt start xs
    in  S.insertAt end value xs'

circularIndex :: Int -> Int -> Int
circularIndex 1 _ = error "div by 0"
circularIndex period i = i `mod` (period - 1)

baseToZero :: [Int] -> [Int]
baseToZero xs =
    let (toWrap, res) = break (==0) xs
    in  res ++ toWrap

groveCoordinates :: [Int] -> [Int]
groveCoordinates xs = map (xs' !!) [1000 `mod` period, 2000 `mod` period, 3000 `mod` period]
    where period = length xs
          xs' = baseToZero xs

positionsToValues :: [Int] -> S.Seq Int -> [Int]
positionsToValues values = map (values !!) . toList