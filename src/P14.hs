module P14 (run1, run2, inputLocation) where

import qualified Data.Set as S
import Data.List.Split (splitOn)
import Data.List (find)
import Data.Tuple (swap)

type Coord = (Int, Int)
type State = S.Set Coord

inputLocation :: String
inputLocation = "inputs/input14"

run1 :: String -> Int
run1 = solve1 . parse1

run2 :: String -> Int
run2 = solve2 . parse1

parse1 :: String -> State
parse1 = S.unions . map parseLine . lines

parseLine :: String -> State
parseLine = S.fromList . concat . zipWithSelf lineCoords . map parseCoords . splitOn " -> "

parseCoords :: String -> (Int, Int)
parseCoords = parseCoords' . splitOn ","
    where parseCoords' (a:b:_) = (read a, read b)
          parseCoords' x = error ("Malformatted coordinate " ++ unwords x)

lineCoords :: Coord -> Coord -> [Coord]
lineCoords (x1,y1) (x2, y2)
    | x1 == x2 = map (pair x1) (numbersBetween y1 y2)
    | y1 == y2 = map (swap . pair y1) (numbersBetween x1 x2)
    | otherwise = error ("Input coords must be in straight line: " ++ show (x1,y1) ++ " " ++ show (x2,y2))

pair :: a -> a -> (a,a)
pair x y = (x,y)

numbersBetween :: Int -> Int -> [Int]
numbersBetween x y
    | x > y = [y..x]
    | otherwise = [x..y]

zipWithSelf :: (a -> a -> b) -> [a] -> [b]
zipWithSelf f xs = zipWith f xs (tail xs)

solve1 :: State -> Int
solve1 state = solve floorLevel floorLevel state
    where floorLevel = chamberFloorLevel state

solve :: Int -> Int -> State -> Int
solve floorLevel targetLevel state
    | snd nextSandLocation == targetLevel = 0
    | otherwise = 1 + solve floorLevel targetLevel state'
    where nextSandLocation = dropSand floorLevel (500,0) state
          state' = S.insert nextSandLocation state

dropSand :: Int -> Coord -> State -> Coord
dropSand floorLevel sandLocation state
    | snd sandLocation == floorLevel = sandLocation
    | otherwise = case openSpaceBeneath state sandLocation of
        Nothing -> sandLocation
        Just space -> dropSand floorLevel space state

openSpaceBeneath :: State -> Coord -> Maybe Coord
openSpaceBeneath state = find (isSpaceEmpty state) . coordsBeneath

isSpaceEmpty :: State -> Coord -> Bool
isSpaceEmpty state = not . (`S.member` state)

coordsBeneath :: Coord -> [Coord]
coordsBeneath (x, y) = [(x, y+1), (x-1, y+1), (x+1, y+1)]

chamberFloorLevel :: State -> Int
chamberFloorLevel = floorLevel' . maxYLevel
    where floorLevel' Nothing = error "Empty room state"
          floorLevel' (Just lowestRock) = lowestRock + 1

maxYLevel :: State -> Maybe Int
maxYLevel = S.lookupMax . S.map snd

solve2 :: S.Set Coord -> Int
solve2 state = 1 + solve (chamberFloorLevel state) 0 state