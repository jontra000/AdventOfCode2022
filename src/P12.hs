module P12 (run1, run2, inputLocation) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char (ord)

type Coord = (Int, Int)
-- height, tentativeDistance
data Node = Node Int (Maybe Int)
type State = M.Map Coord Node
-- 'S' coord, 'E' coord, grid
data Input = Input Coord Coord State

inputLocation :: String
inputLocation = "inputs/input12"

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

heightGrid :: M.Map Coord Char -> State
heightGrid = M.map (constructNode . charToHeight)

constructNode :: Int -> Node
constructNode nodeHeight = Node nodeHeight Nothing

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
-- we look in reverse to make part 2 easier
solve1 (Input start end grid) = solveDijkstra grid (Just (end, (25,0))) start

solveDijkstra :: State -> Maybe (Coord, (Int, Int)) -> Coord -> Maybe Int
solveDijkstra _ Nothing _ = Nothing
solveDijkstra unvisitedNodes (Just (currentNode, (currentHeight, currentDistance))) targetNode
    | currentNode == targetNode = Just currentDistance
    | otherwise =
        let unvisitedNodes' = updateUnvisited Nothing currentHeight (currentDistance+1) currentNode unvisitedNodes
            nextNode = smallestDistance unvisitedNodes'
        in  solveDijkstra unvisitedNodes' nextNode targetNode

updateUnvisited :: Maybe Int -> Int -> Int -> Coord -> State -> State
updateUnvisited bestDistance currentHeight nextDistance currentCoord grid
    | maybe False (<= nextDistance) bestDistance = M.delete currentCoord grid
    | otherwise = M.delete currentCoord $ updateNeighbours currentHeight nextDistance grid currentCoord

solveDijkstraMulti :: Maybe Int -> State -> Maybe (Coord, (Int, Int)) -> S.Set Coord -> Maybe Int
solveDijkstraMulti bestDistance _ Nothing _ = bestDistance
solveDijkstraMulti bestDistance unvisitedNodes  (Just (currentNode, (currentHeight, currentDistance))) targetNodes
    | null targetNodes = bestDistance
    | otherwise =
        let unvisitedNodes' = updateUnvisited bestDistance currentHeight (currentDistance+1) currentNode unvisitedNodes
            bestDistance' = if isNewBestDistance currentHeight bestDistance currentDistance then Just currentDistance else bestDistance
            targetNodes' = S.delete currentNode targetNodes
            nextNode = smallestDistance unvisitedNodes'
        in  solveDijkstraMulti bestDistance' unvisitedNodes' nextNode targetNodes'

isNewBestDistance :: Int -> Maybe Int -> Int -> Bool
isNewBestDistance 0 Nothing _ = True
isNewBestDistance 0 (Just oldValue) newValue = newValue < oldValue
isNewBestDistance _ _ _ = False 

smallestDistance :: State -> Maybe (Coord, (Int, Int))
smallestDistance = M.foldlWithKey smallestDistance' Nothing
    where smallestDistance' Nothing c (Node h (Just d)) = Just (c, (h, d))
          smallestDistance' prev@(Just (_, (_, oldDistance))) c (Node h (Just newDistance))
            | oldDistance > newDistance = Just (c, (h, newDistance))
            | otherwise = prev
          smallestDistance' prev _ _ = prev

updateNeighbours :: Int -> Int -> State -> Coord -> State
updateNeighbours currentHeight currentDistance grid = foldl (updateNeighbour currentDistance) grid . reachableNeighbours currentHeight grid

reachableNeighbours :: Int -> State -> Coord -> [Coord]
reachableNeighbours currentHeight grid = M.keys . M.filter (heightIsReachable currentHeight) . M.restrictKeys grid . adjacentCoords

heightIsReachable :: Int -> Node -> Bool
heightIsReachable currentHeight (Node nodeHeight _) = nodeHeight > currentHeight - 2

adjacentCoords :: Coord -> S.Set Coord
adjacentCoords (x,y) = S.fromList [(x+1, y), (x-1,y), (x,y+1), (x,y-1)]

updateNeighbour :: Int -> State -> Coord -> State
updateNeighbour currentDistance grid coord = M.adjust (updateDistance currentDistance) coord grid

updateDistance :: Int -> Node -> Node
updateDistance newDistance (Node h oldDistance) = Node h (updateDistance' newDistance oldDistance)
    where updateDistance' newValue = Just . maybe newValue (min newValue)

solve2 :: Input -> Maybe Int
solve2 (Input _ end grid) = solveDijkstraMulti Nothing grid (Just (end, (25,0))) (lowestCoords grid)

lowestCoords :: State -> S.Set Coord
lowestCoords = S.fromList . M.keys . M.filter (heightEquals 0)

heightEquals :: Int -> Node -> Bool
heightEquals x (Node height _) = height == x