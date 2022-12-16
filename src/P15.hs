module P15 (run1, run2, inputLocation) where

import Data.List (sortBy, nub)
import Data.Maybe (mapMaybe)
import Data.Function (on)

type Coord = (Int, Int)
-- sensors, beacons
type SensorBeaconPair = (Coord, Coord)
data SensorDistancePair = SensorDistancePair Coord Int deriving Show
type State = [SensorBeaconPair]
type State2 = [SensorDistancePair]

inputLocation :: String
inputLocation = "inputs/input15"

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse2

parse :: String -> State
parse = map parseLine . lines

parse2 :: String -> [SensorDistancePair]
parse2 = map distancePair . parse

distancePair :: SensorBeaconPair -> SensorDistancePair
distancePair (sensor, beacon) = SensorDistancePair sensor (manhattanDistance sensor beacon)

parseLine :: String -> (Coord, Coord)
parseLine s =
    let ws = words s
    in  (parseCoords (ws !! 2) (ws !! 3), parseCoords (ws !! 8) (ws !! 9))

parseCoords :: String -> String -> Coord
parseCoords xs ys = (read (filter isNumber xs), read (filter isNumber ys))

isNumber :: Char -> Bool
isNumber '0' = True
isNumber '1' = True
isNumber '2' = True
isNumber '3' = True
isNumber '4' = True
isNumber '5' = True
isNumber '6' = True
isNumber '7' = True
isNumber '8' = True
isNumber '9' = True
isNumber '-' = True
isNumber _ = False

solve1 :: State -> Int
solve1 state = stripBeacons 2000000 state $ coverageOnRow 2000000 state

coverageOnRow :: Int -> State -> Int
coverageOnRow row = coverageOnRow' . sortBy (compare `on` fst) . mapMaybe (slice row . distancePair)
      where coverageOnRow' [] = 0
            coverageOnRow' ((leftEdge,rightEdge):slices) = coverageOnRow'' leftEdge rightEdge slices
            coverageOnRow'' left right [] = right - left + 1
            coverageOnRow'' left right allSlices@((nextLeft, nextRight):slices) =
                if nextLeft <= right+1 then
                    coverageOnRow'' left (max right nextRight) slices
                else
                    right + 1 - left + coverageOnRow' allSlices

beaconsOn :: Int -> State -> Int
beaconsOn row = length . nub . filter ((==row) . snd) . map snd

stripBeacons :: Int -> State -> Int -> Int
stripBeacons row state x = x - beaconsOn row state

pair :: Int -> Int -> Coord
pair x y = (x,y)

manhattanDistance :: Coord -> Coord -> Int
manhattanDistance (x1,y1) (x2,y2) = abs (x1 - x2) + abs (y1 - y2)

solve2 :: State2 -> Int
solve2 = tuningFrequency . findSensor

findSensor :: State2 -> Coord
findSensor state = head $ mapMaybe (checkRow state) [0..4000000]

tuningFrequency :: Coord -> Int
tuningFrequency (x, y) = x*4000000 + y

checkRow :: State2 -> Int -> Maybe Coord
checkRow state y =
    let slices = sortBy (compare `on` fst) $ mapMaybe (slice y) state
    in  (`pair` y) <$> checkSlices 0 slices

checkSlices :: Int -> [(Int, Int)] -> Maybe Int
checkSlices rightEdge slices
    | rightEdge > 4000000 = Nothing
    | otherwise = case slices of
        [] -> Just rightEdge
        ((nextLeft, nextRight):xs) -> if nextLeft <= rightEdge
            then checkSlices (max rightEdge (nextRight + 1)) xs
            else Just rightEdge

slice :: Int -> SensorDistancePair -> Maybe (Int, Int)
slice row (SensorDistancePair (x,y) distance) =
    if xDistance < 0
    then Nothing
    else Just (x - xDistance, x + xDistance)
    where xDistance = distance - abs (y - row)