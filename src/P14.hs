module P14 (run1, run2, inputLocation) where

import qualified Data.Set as S
import Data.List.Split (splitOn)
import qualified Data.Maybe

type Coord = (Int, Int)

inputLocation :: String
inputLocation = "inputs/input14"

run1 :: String -> Int
run1 = solve1 . parse1

run2 :: String -> Int
run2 = solve2 . parse1

parse1 :: String -> S.Set Coord
parse1 = S.unions . map parseLine . lines

parseLine :: String -> S.Set Coord
parseLine = S.fromList . concat . zipWithSelf lineCoords . map parseCoords . splitOn " -> "

parseCoords :: String -> (Int, Int)
parseCoords = parseCoords' . splitOn ","
    where parseCoords' (a:b:_) = (read a, read b)
          parseCoords' x = error ("Malformatted coordinate " ++ unwords x)

lineCoords :: Coord -> Coord -> [Coord]
lineCoords (x1,y1) (x2, y2)
    | x1 == x2 = map (\y -> (x1, y)) (numbersBetween y1 y2)
    | y1 == y2 = map (\x -> (x, y1)) (numbersBetween x1 x2)
    | otherwise = error ("Input coords must be in straight line: " ++ show (x1,y1) ++ " " ++ show (x2,y2))

numbersBetween :: Int -> Int -> [Int]
numbersBetween x y
    | x > y = [y..x]
    | otherwise = [x..y]

zipWithSelf :: (a -> a -> b) -> [a] -> [b]
zipWithSelf f xs = zipWith f xs (tail xs)

solve1 :: S.Set Coord -> Int
solve1 state = solve1' 0 (Data.Maybe.fromJust $ S.lookupMax (S.map snd state)) state

solve1' :: Int -> Int -> S.Set Coord -> Int
solve1' count lowestRock state = case nextSandLoc lowestRock state of
    Nothing -> count
    Just sandLoc -> solve1' (count+1) lowestRock (S.insert sandLoc state)

nextSandLoc :: Int -> S.Set Coord -> Maybe Coord
nextSandLoc lowestRock state = nextSandLoc' lowestRock state (500, 0)

nextSandLoc' :: Int -> S.Set Coord -> Coord -> Maybe Coord
nextSandLoc' lowestRock state sandLoc@(sandX, sandY)
    | sandY > lowestRock = Nothing
    | otherwise = dropSand lowestRock state [(sandX, sandY+1), (sandX-1, sandY+1), (sandX+1, sandY+1), sandLoc]

dropSand :: Int -> S.Set Coord -> [Coord] -> Maybe Coord
dropSand _ _ [x] = Just x
dropSand _ _ [] = error "Should not reach"
dropSand lowestRock state (nextLoc:locs) =
    if S.member nextLoc state then
        dropSand lowestRock state locs
    else
        nextSandLoc' lowestRock state nextLoc

solve2 :: S.Set Coord -> Int
solve2 state = solve2' 0 (Data.Maybe.fromJust $ S.lookupMax (S.map snd state)) state

solve2' :: Int -> Int -> S.Set Coord -> Int
solve2' count lowestRock state = case nextSandLoc2 lowestRock state (500, 0) of
    (500, 0) -> count+1
    sandLoc -> solve2' (count+1) lowestRock (S.insert sandLoc state)

nextSandLoc2 :: Int -> S.Set Coord -> Coord -> Coord
nextSandLoc2 lowestRock state sandLoc@(sandX, sandY)
    | sandY > lowestRock = sandLoc
    | otherwise = dropSand2 lowestRock state [(sandX, sandY+1), (sandX-1, sandY+1), (sandX+1, sandY+1), sandLoc]

dropSand2 :: Int -> S.Set Coord -> [Coord] -> Coord
dropSand2 _ _ [x] = x
dropSand2 _ _ [] = error "Should not reach"
dropSand2 lowestRock state (nextLoc:locs) =
    if S.member nextLoc state then
        dropSand2 lowestRock state locs
    else
        nextSandLoc2 lowestRock state nextLoc