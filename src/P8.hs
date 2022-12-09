module P8 (run1, run2) where

import qualified Data.Map as M
import Data.Char (digitToInt)
import Data.List (nub, transpose)
import Data.Tuple (swap)

type TreeGrid1 = [[Int]]
type TreeGrid2 = M.Map (Int, Int) Int

run1 :: String -> Int
run1 = solve1 . parse1

run2 :: String -> Int
run2 = solve2 . parse2

parse1 :: String -> TreeGrid1
parse1 = map (map digitToInt) . lines

solve1 :: TreeGrid1 -> Int
solve1 input = length $ overlaySolutions (solveRows input) (solveRows $ transpose input)

solveRows :: TreeGrid1 -> [(Int, Int)]
solveRows = concat . zipWith addY [0..] . map solveRow

solveRow :: [Int] -> [Int]
solveRow row =
    let solveLeft = solveRow' row (-1) 0 (+ 1)
        solveRight = solveRow' (reverse row) (-1) (length row - 1) (+ (-1))
    in  nub $ solveLeft ++ solveRight

solveRow' :: [Int] -> Int -> Int -> (Int -> Int) -> [Int]
solveRow' [] _ _ _= []
solveRow' [_] _ i _ = [i]
solveRow' (9:_) _ i _ = [i]
solveRow' (x:xs) prevHeight i inc =
    let i' = inc i
    in  if x > prevHeight then i : solveRow' xs x i' inc else solveRow' xs prevHeight i' inc

addY :: Int -> [Int] -> [(Int, Int)]
addY y = map (\x -> (x,y))

overlaySolutions :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
overlaySolutions horizontals verticals = nub $ horizontals ++ map swap verticals

parse2 :: String -> TreeGrid2
parse2 = M.fromList . concat . zipWith parseLine [0..] . lines

parseLine :: Int -> String -> [((Int, Int), Int)]
parseLine y = zipWith (\x c -> ((x, y), digitToInt c)) [0..]

solve2 :: TreeGrid2 -> Int
solve2 = maximum . scenicScores

scenicScores :: TreeGrid2 -> [Int]
scenicScores grid = map (scenicScore grid) $ M.toList grid 

scenicScore :: TreeGrid2 -> ((Int, Int), Int) -> Int
scenicScore grid (i, height) = scoreNorth grid height i * scoreSouth grid height i * scoreWest grid height i * scoreEast grid height i

scoreEast :: TreeGrid2 -> Int -> (Int, Int) -> Int
scoreEast = score (\(x,y) -> (x+1, y))

scoreWest :: TreeGrid2 -> Int -> (Int, Int) -> Int
scoreWest = score (\(x,y) -> (x-1, y))

scoreSouth :: TreeGrid2 -> Int -> (Int, Int) -> Int
scoreSouth = score (\(x,y) -> (x, y+1))

scoreNorth :: TreeGrid2 -> Int -> (Int, Int) -> Int
scoreNorth = score (\(x,y) -> (x, y-1))

score :: ((Int, Int) -> (Int, Int)) -> TreeGrid2 -> Int -> (Int, Int) -> Int
score inc grid height i =
    case M.lookup i' grid of
        Nothing -> 0
        Just x -> if x < height then 1 + score inc grid height i' else 1
    where i' = inc i