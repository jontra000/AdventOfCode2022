module P12 (run1, run2) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char (ord)
import Data.Foldable (minimumBy)
import Data.Function (on)
import Data.Maybe (mapMaybe)

type Coord = (Int, Int)
data Node = Node {height :: Int, tentativeDistance :: Maybe Int}
type Grid = M.Map Coord Node
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
heightGrid = M.map ((`Node` Nothing) . charToHeight)

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
solve1 (Input start end grid) = solveDijkstra grid (end, (25,0)) start

solveDijkstra :: Grid -> (Coord, (Int, Int)) -> Coord -> Maybe Int
solveDijkstra unvisitedNodes (currentNode, (currentHeight, currentDistance)) targetNode
    | currentNode == targetNode = tentativeDistance (unvisitedNodes M.! currentNode)
    | otherwise =
        let unvisitedNodes' = M.delete currentNode $ updateNeighbours currentHeight (currentDistance+1) unvisitedNodes currentNode
            nextNode = smallestDistance unvisitedNodes'
        in  nextNode >>= (\x -> solveDijkstra unvisitedNodes' x targetNode)

solveDijkstraMulti :: Maybe Int -> Grid -> (Coord, (Int, Int)) -> S.Set Coord -> Maybe Int
solveDijkstraMulti bestDistance unvisitedNodes (currentNode, (currentHeight, currentDistance)) targetNodes
    | null targetNodes = bestDistance
    | otherwise =
        let unvisitedNodes' = updateNeighbours currentHeight (currentDistance+1) unvisitedNodes currentNode
            unvisitedNodes'' = M.delete currentNode unvisitedNodes'
            bestDistance' = if currentHeight == 0 && maybe True (>= currentDistance) bestDistance then Just currentDistance else bestDistance
            targetNodes' = S.delete currentNode targetNodes
        in  case smallestDistance unvisitedNodes''  of
                Nothing -> bestDistance'
                Just nextNode -> solveDijkstraMulti bestDistance' unvisitedNodes'' nextNode targetNodes'

smallestDistance :: Grid -> Maybe (Coord, (Int, Int))
smallestDistance = safeMinimum . mapMaybe nodesWithDistance . M.toList
    where safeMinimum [] = Nothing
          safeMinimum xs = Just (minimumBy (compare `on` (snd .snd)) xs)
          nodesWithDistance (_, Node _ Nothing) = Nothing
          nodesWithDistance (c, Node h (Just d)) = Just (c, (h,d))

updateNeighbours :: Int -> Int -> Grid -> Coord -> Grid
updateNeighbours currentHeight currentDistance grid = foldl (updateNeighbour currentDistance) grid . reachableNeighbours currentHeight grid

reachableNeighbours :: Int -> Grid -> Coord -> [Coord]
reachableNeighbours currentHeight grid = M.keys . M.filter ((> currentHeight - 2) . height) . M.restrictKeys grid . adjacentCoords

adjacentCoords :: Coord -> S.Set Coord
adjacentCoords (x,y) = S.fromList [(x+1, y), (x-1,y), (x,y+1), (x,y-1)]

updateNeighbour :: Int -> Grid -> Coord -> Grid
updateNeighbour currentDistance grid coord = M.update (updateDistance currentDistance) coord grid

updateDistance :: Int -> Node -> Maybe Node
updateDistance newDistance (Node h oldDistance) = Just $ Node h (updateDistance' newDistance oldDistance)
    where updateDistance' newValue = Just . maybe newValue (min newValue)

solve2 :: Input -> Maybe Int
solve2 (Input _ end grid) = solveDijkstraMulti Nothing grid (end, (25,0)) (lowestCoords grid)

lowestCoords :: Grid -> S.Set Coord
lowestCoords = S.fromList . M.keys . M.filter ((==0) . height)