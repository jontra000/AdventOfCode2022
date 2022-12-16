module P15 (run1, run2, inputLocation) where

import Data.List (sortBy, nub)
import Data.Maybe (mapMaybe)
import Data.Function (on)

type Coord = (Int, Int)
type Slice = (Int, Int)
-- sensors, beacons, distance
data SensorBeaconPair = SensorBeaconPair Coord Coord Int
type State = [SensorBeaconPair]

inputLocation :: String
inputLocation = "inputs/input15"

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

parse :: String -> State
parse = map parseLine . lines

distancePair :: Coord -> Coord -> SensorBeaconPair
distancePair sensor beacon = SensorBeaconPair sensor beacon (manhattanDistance sensor beacon)

parseLine :: String -> SensorBeaconPair
parseLine s =
    let ws = words s
    in  distancePair (parseCoords (init (ws !! 2)) (init (ws !! 3))) (parseCoords (init (ws !! 8)) (ws !! 9))

parseCoords :: String -> String -> Coord
parseCoords xs ys = (parseNumber xs, parseNumber ys)

parseNumber :: String -> Int
parseNumber = read . drop 2

targetRowPart1 :: Int
targetRowPart1 = 2000000

solve1 :: State -> Int
solve1 state = removeBeacons targetRowPart1 state $ coverageOnRow targetRowPart1 state

orderedSlicesAtRow :: Int -> State -> [Slice]
orderedSlicesAtRow row = sortBy (compare `on` fst) . mapMaybe (slice row)

coverageOnRow :: Int -> State -> Int
coverageOnRow row = coverageOnSlices . orderedSlicesAtRow row

coverageOnSlices :: (Num p, Ord p) => [(p, p)] -> p
coverageOnSlices [] = 0
coverageOnSlices ((leftEdge,rightEdge):slices) = coverageOnSlices' leftEdge rightEdge slices

coverageOnSlices' :: (Num a, Ord a) => a -> a -> [(a, a)] -> a
coverageOnSlices' left right [] = right - left + 1
coverageOnSlices' left right ((nextLeft, nextRight):slices) =
    if nextLeft <= right + 1 then
        coverageOnSlices' left (max right nextRight) slices
    else
        right + 1 - left + coverageOnSlices' nextLeft nextRight slices

beaconsOn :: Int -> State -> Int
beaconsOn row = countUnique . filter (isOnRow row) . map getBeacon

isOnRow :: Int -> Coord -> Bool
isOnRow row = (==row) . snd

countUnique :: Eq a => [a] -> Int
countUnique = length . nub

getBeacon :: SensorBeaconPair -> Coord
getBeacon (SensorBeaconPair _ x _) = x

removeBeacons :: Int -> State -> Int -> Int
removeBeacons row state x = x - beaconsOn row state

pair :: Int -> Int -> Coord
pair x y = (x,y)

manhattanDistance :: Coord -> Coord -> Int
manhattanDistance (x1,y1) (x2,y2) = abs (x1 - x2) + abs (y1 - y2)

solve2 :: State -> Int
solve2 = tuningFrequency . findSensor

maxCoordValue :: Int
maxCoordValue = 4000000

findSensor :: State -> Coord
findSensor state = head $ mapMaybe (`coverageGap` state) [0..maxCoordValue]

tuningFrequency :: Coord -> Int
tuningFrequency (x, y) = x*4000000 + y

coverageGap :: Int -> State -> Maybe Coord
coverageGap y = fmap (`pair` y) . coverageGapSlices 0 . orderedSlicesAtRow y

coverageGapSlices :: Int -> [Slice] -> Maybe Int
coverageGapSlices rightEdge slices
    | rightEdge > maxCoordValue = Nothing
    | otherwise = coverageGapSlices' rightEdge slices

coverageGapSlices' :: Int -> [Slice] -> Maybe Int
coverageGapSlices' rightEdge [] = Just rightEdge
coverageGapSlices' rightEdge ((nextLeft, nextRight):xs)
    | nextLeft > rightEdge = Just rightEdge
    | otherwise = coverageGapSlices rightEdge' xs
        where rightEdge' = max rightEdge (nextRight + 1)

slice :: Int -> SensorBeaconPair -> Maybe (Int, Int)
slice row (SensorBeaconPair (x,y) _ distance)
    | xDistance < 0 = Nothing
    | otherwise = Just (x - xDistance, x + xDistance)
        where xDistance = distance - abs (y - row)