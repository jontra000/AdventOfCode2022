module P12 (run1, run2) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char (ord)
import Data.Foldable (minimumBy)
import Data.Function (on)

type Coord = (Int, Int)
type Grid = M.Map Coord Int
data Input = Input {inputStart :: Coord, inputEnd :: Coord, inputGrid :: Grid}

run1 :: String -> Maybe Int
run1 = solve1 . parse

run2 :: String -> Maybe Int
run2 = solve2 . parse

parse :: String -> Input
parse = input . M.fromList . concat . zipWith parseRow [0..] . lines

parseRow :: Int -> String -> [(Coord, Char)]
parseRow y = zipWith (parseChar y) [0..]

parseChar :: Int -> Int -> Char -> (Coord, Char)
parseChar y x c = ((x,y), c)

input :: M.Map Coord Char -> Input
input charGrid = Input (startCoord charGrid) (endCoord charGrid) (heightGrid charGrid)

heightGrid :: M.Map Coord Char -> Grid
heightGrid = M.map charToHeight

charToHeight :: Char -> Int
charToHeight 'S' = 0
charToHeight 'E' = 25
charToHeight c = ord c - 97

startCoord :: M.Map Coord Char -> Coord
startCoord = findChar 'S'

endCoord :: M.Map Coord Char -> Coord
endCoord = findChar 'E'

findChar :: Char -> M.Map Coord Char -> Coord
findChar c = fst . head . M.toList . M.filter (==c)

solve1 :: Input -> Maybe Int
solve1 (Input start end grid) = solveDijkstra grid (M.singleton start 0) (start, 0) end

solveDijkstra :: Grid -> Grid -> (Coord, Int) -> Coord -> Maybe Int
solveDijkstra unvisitedNodes tentativeDistances (currentNode, currentDistance) targetNode
    | currentNode == targetNode = Just (tentativeDistances M.! currentNode)
    | otherwise =
        let tentativeDistances' = updateNeighbours tentativeDistances (currentDistance+1) unvisitedNodes currentNode
            unvisitedNodes' = M.delete currentNode unvisitedNodes
            nextNode = smallestDistance tentativeDistances' unvisitedNodes' 
        in  nextNode >>= (\x -> solveDijkstra unvisitedNodes' tentativeDistances' x targetNode)

smallestDistance :: Grid -> Grid -> Maybe (Coord, Int)
smallestDistance tentativeDistances = safeMinimum . M.toList . M.intersection tentativeDistances
    where safeMinimum [] = Nothing
          safeMinimum xs = Just (minimumBy (compare `on` snd) xs)

updateNeighbours :: Grid -> Int -> Grid -> Coord -> Grid
updateNeighbours tentativeDistances currentDistance grid = foldl (updateNeighbour currentDistance) tentativeDistances . reachableNeighbours grid

reachableNeighbours :: Grid -> Coord -> [Coord]
reachableNeighbours grid coord = M.keys $ M.filter (< height + 2) $ M.restrictKeys grid $ adjacentCoords coord
    where height = grid M.! coord

adjacentCoords :: Coord -> S.Set Coord
adjacentCoords (x,y) = S.fromList [(x+1, y), (x-1,y), (x,y+1), (x,y-1)]

updateNeighbour :: Int -> Grid -> Coord -> Grid
updateNeighbour currentDistance grid coord = M.alter (updateDistance currentDistance) coord grid

updateDistance :: Int -> Maybe Int -> Maybe Int
updateDistance x Nothing = Just x
updateDistance x (Just y) = Just (min x y)

solve2 :: Input -> Maybe Int
solve2 = minimum . filter (/= Nothing) . map solve1 . hikeStarts

hikeStarts :: Input -> [Input]
hikeStarts (Input _ end grid) = map (\start -> Input start end grid) $ lowestCoords grid

lowestCoords :: Grid -> [Coord]
lowestCoords = M.keys . M.filter (==0)