module P18 (run1, run2, inputLocation) where

import Data.List.Split (splitOn)
import qualified Data.Set as S
import Data.Maybe (fromMaybe)

inputLocation :: String
inputLocation = "inputs/input18"

type Coord = (Int, Int, Int)
type State = S.Set Coord

run1 :: String -> Int
run1 = solve . parse

run2 :: String -> Int
run2 = solve . fillGaps . parse

parse :: String -> State
parse = S.fromList . map parseCoord . lines

parseCoord :: String -> Coord
parseCoord = parseCoord' . map read . splitOn ","
    where parseCoord' (x:y:z:_) = (x,y,z)
          parseCoord' xs = error ("Can't parse coords " ++ unwords (map show xs))

solve :: State -> Int
solve state = sum $ map (openSides state) $ S.toList state

openSides :: State -> Coord -> Int
openSides state = length . filter (`S.notMember` state) . neighbours

neighbours :: Coord -> [Coord]
neighbours (x,y,z) = [(x+1, y , z), (x-1, y, z), (x, y+1, z), (x, y-1, z), (x, y, z+1), (x, y, z-1)]

fillGaps :: State -> State
fillGaps state =
        let (maxX, maxY, maxZ) = maxValues state
            isInBounds = (\(x,y,z) -> x >= 0 && x <= maxX && y >= 0 && y <= maxY && z >= 0 && z <= maxZ)
            allCoords = [(x,y,z) | x <- [0..maxX], y <- [0..maxY], z <- [0..maxZ]]
        in  foldl (floodFill isInBounds) state allCoords

maxValues :: State -> (Int, Int, Int)
maxValues state = 
    case (S.lookupMax $ S.map (\(x,_,_) -> x) state, S.lookupMax $ S.map (\(_,y,_) -> y) state, S.lookupMax $ S.map (\(_,_,z) -> z) state) of
        (Just maxX, Just maxY, Just maxZ) -> (maxX, maxY, maxZ)
        _ -> error "Empty state"

floodFill :: (Coord -> Bool) -> State -> Coord -> State
floodFill isInBounds state = fromMaybe state . floodFillCell isInBounds state . singleton

floodFillCell :: (Coord -> Bool) -> State -> [Coord] -> Maybe State
floodFillCell _ state [] = Just state
floodFillCell isInBounds state (c:cs)
    | S.member c state = floodFillCell isInBounds state cs
    | isInBounds c = floodFillCell isInBounds (S.insert c state) (neighbours c ++ cs)
    | otherwise = Nothing

singleton :: a -> [a]
singleton = (: [])