module P24 (run1, run2, inputLocation) where

import qualified Data.Set as S
import Data.Maybe (catMaybes)
import GHC.OldList (findIndex)

type Coord = (Int, Int)
data Heading = North | South | East | West deriving (Eq, Ord)
data Blizzard = Blizzard Coord Heading deriving (Eq, Ord)
type FreeCoords = S.Set Coord

inputLocation :: String
inputLocation = "inputs/input24"

run1 :: String -> Maybe Int
run1 = solve1 . parse

run2 :: String -> Maybe Int
run2 = solve2 . parse

parse :: String -> [Blizzard]
parse = concat . zipWith parseLine [0..] . lines

parseLine :: Int -> String -> [Blizzard]
parseLine y = catMaybes . zipWith (parseChar y) [0..]

parseChar :: Int -> Int -> Char -> Maybe Blizzard
parseChar y x '<' = Just $ Blizzard (x,y) West
parseChar y x '>' = Just $ Blizzard (x,y) East
parseChar y x '^' = Just $ Blizzard (x,y) North
parseChar y x 'v' = Just $ Blizzard (x,y) South
parseChar _ _ _ = Nothing

ySize :: Int
ySize = 35

xSize :: Int
xSize = 100

target :: Coord
target = (xSize, ySize+1)

solve1 :: [Blizzard] -> Maybe Int
solve1 = fmap ((-1) +) . pathTime (1,0) target . openSpaces

solve2 :: [Blizzard] -> Maybe Int
solve2 blizzards = do
    timeToTarget <- pathTime (1,0) target openSpaces'
    timeToOrigin <- pathTimeWithOffset timeToTarget target (1,0) openSpaces'
    res <- pathTimeWithOffset timeToOrigin (1,0) target openSpaces'
    Just (res - 1)
        where openSpaces' = openSpaces blizzards

pathTimeWithOffset :: Int -> Coord -> Coord -> [FreeCoords] -> Maybe Int
pathTimeWithOffset offset start end = fmap (offset +) . pathTime start end . drop offset

pathTime :: Coord -> Coord -> [FreeCoords] -> Maybe Int
pathTime start end = findIndex (S.member end) . possiblePaths start

possiblePaths :: Coord -> [FreeCoords] -> [S.Set Coord]
possiblePaths start = scanl iteratePaths (S.singleton start)

iteratePaths :: S.Set Coord -> FreeCoords -> S.Set Coord
iteratePaths possibleLocations freeLocations =
    let possibleLocations' = S.unions $ S.map possibleMoves possibleLocations
    in  S.intersection possibleLocations' freeLocations

possibleMoves :: Coord -> S.Set Coord
possibleMoves c@(x,y) = S.fromList [(x+1,y), (x,y+1), (x-1,y), (x,y-1), c]

blizzardLocation :: Blizzard -> Coord
blizzardLocation (Blizzard location _) = location

openSpaces :: [Blizzard] -> [FreeCoords]
openSpaces = map toFreeCoords . iterate moveBlizzards

gridCoords :: S.Set Coord
gridCoords = S.fromList ((1,0) : target : [(x,y) | x <- [1..xSize], y <- [1..ySize]])

toFreeCoords :: [Blizzard] -> FreeCoords
toFreeCoords  = S.difference gridCoords . occupiedCoords

occupiedCoords :: [Blizzard] -> S.Set Coord
occupiedCoords = S.fromList . map blizzardLocation

moveBlizzards :: [Blizzard] -> [Blizzard]
moveBlizzards = map (wrapBlizzard . moveBlizzard)

moveBlizzard :: Blizzard -> Blizzard
moveBlizzard (Blizzard (x,y) North) = Blizzard (x, y-1) North
moveBlizzard (Blizzard (x,y) South) = Blizzard (x, y+1) South
moveBlizzard (Blizzard (x,y) East) = Blizzard (x+1, y) East
moveBlizzard (Blizzard (x,y) West) = Blizzard (x-1, y) West

wrapBlizzard :: Blizzard -> Blizzard
wrapBlizzard blizzard@(Blizzard (x,y) heading)
    | x <= 0 = Blizzard (xSize, y) heading
    | x > xSize = Blizzard (1, y) heading
    | y <= 0 = Blizzard (x, ySize) heading
    | y > ySize = Blizzard (x, 1) heading
    | otherwise = blizzard