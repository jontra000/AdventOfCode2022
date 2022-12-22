module P16 (run1, run2, inputLocation) where

import qualified Data.Map as M
import Data.List (delete, (\\))
import Data.Maybe (mapMaybe)

-- pressure, neighbours
data Valve = Valve Int [String]
type Input = M.Map String Valve

-- name, pressure, distance to nodes
type DistanceMap = M.Map String Int
data Node = Node Int DistanceMap deriving (Eq, Ord, Show)
type Network = M.Map String Node

type UnvisitedNodes = M.Map String (Maybe Int)

inputLocation :: String
inputLocation = "inputs/input16"

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

parse :: String -> Input
parse = M.fromList . map parseLine . lines

parseLine :: String -> (String, Valve)
parseLine s =
    let ws = words s
        name = ws !! 1
        pressure = parsePressure (ws !! 4)
        neighbours = map parseNeighbour (drop 9 ws)
    in  (name, Valve pressure neighbours)

parseNeighbour :: String -> String
parseNeighbour = take 2

parsePressure :: String -> Int
parsePressure = read . drop 5 . init

solve1 :: Input -> Int
solve1 = findMaxPressure 30 . nodePaths

solve2 :: Input -> Int
solve2 = findMaxPressure2 26 . nodePaths

nodePaths :: Input -> (DistanceMap, [(String, Node)])
nodePaths input =
    let valves = M.filter (> 0) $ M.map (\(Valve pressure _) -> pressure) input
        network = M.elems $ M.mapWithKey (node input (M.keys valves)) valves
        initDistances = distances input (M.keys valves) "AA"
    in  (initDistances, network)

distances :: Input -> [String] -> String -> M.Map String Int
distances input destinations start = M.fromList $ zip destinations $ mapMaybe (distance input start) destinations

distance :: Input -> String -> String -> Maybe Int
distance input start = dijkstra input (M.fromList $ map (\k -> (k, Nothing)) $ M.keys input) (Just (start, 0))

dijkstra :: Input -> UnvisitedNodes -> Maybe (String, Int) -> String -> Maybe Int
dijkstra _ _ Nothing _ = Nothing
dijkstra input unvisitedNodes (Just (currentNode, currentDistance)) targetNode
    | currentNode == targetNode = Just currentDistance
    | otherwise =
        let unvisitedNodes' = updateUnvisited input (currentDistance+1) currentNode unvisitedNodes
            nextNode = smallestDistance unvisitedNodes'
        in  dijkstra input unvisitedNodes' nextNode targetNode

smallestDistance :: UnvisitedNodes -> Maybe (String, Int)
smallestDistance = M.foldlWithKey minTentativeDistance Nothing

minTentativeDistance :: Maybe (String, Int) -> String -> Maybe Int -> Maybe (String, Int)
minTentativeDistance prev _ Nothing = prev
minTentativeDistance Nothing key (Just tentativeDistance) = Just (key, tentativeDistance)
minTentativeDistance prev@(Just (_, prevDistance)) key (Just tentativeDistance)
    | tentativeDistance < prevDistance = Just (key, tentativeDistance)
    | otherwise = prev

updateUnvisited :: Input -> Int -> String -> UnvisitedNodes -> UnvisitedNodes
updateUnvisited input nextDistance currentNode unvisitedNodes = M.delete currentNode $ updateNeighbours input nextDistance unvisitedNodes currentNode

updateNeighbours :: Input -> Int -> UnvisitedNodes -> String -> UnvisitedNodes
updateNeighbours valves currentDistance unvisitedNodes = foldl (updateNeighbour currentDistance) unvisitedNodes . reachableNeighbours valves

reachableNeighbours :: Input -> String -> [String]
reachableNeighbours input node = case M.lookup node input of
    Nothing -> error ("error looking up node " ++ node)
    Just x -> neighbours x

neighbours :: Valve -> [String]
neighbours (Valve _ n) = n

updateNeighbour :: Int -> UnvisitedNodes -> String -> UnvisitedNodes
updateNeighbour currentDistance unvisitedNodes key =
    M.adjust (updateDistance currentDistance) key unvisitedNodes

updateDistance :: Int -> Maybe Int -> Maybe Int
updateDistance d Nothing = Just d
updateDistance dNew (Just dOld) = Just (min dNew dOld)

node :: Input -> [String] -> String -> Int -> (String, Node)
node input destinations start pressure = (start, Node pressure $ distances input (delete start destinations) start)

findMaxPressure :: Int -> (DistanceMap, [(String, Node)]) -> Int
findMaxPressure _ (_, []) = 0
findMaxPressure timeLimit (initDistances, nodes) = totalPressure initDistances timeLimit (M.fromList nodes)

findMaxPressure2 :: Int -> (DistanceMap, [(String, Node)]) -> Int
findMaxPressure2 timeLimit (initDistances, network) = maximum $ map (maxPressureForPermutation timeLimit initDistances) $ permute network

maxPressureForPermutation :: Int -> DistanceMap -> ([(String, Node)], [(String, Node)]) -> Int
maxPressureForPermutation timeLimit initDistances (valves1, valves2) =
    totalPressure initDistances timeLimit (M.fromList valves1) + totalPressure initDistances timeLimit (M.fromList valves2)

permute :: Ord a => [a] -> [([a], [a])]
permute xs = removeDuplicateComplements $ map (pairWithComplement xs) $ partitions xs

partitions :: [a] -> [[a]]
partitions [] = []
partitions [x] = [[x],[]]
partitions (x:xs) =
    let xs' = partitions xs
    in  xs' ++ map (x :) xs'

pairWithComplement :: Eq a => [a] -> [a] -> ([a],[a])
pairWithComplement xs xs' = (xs', xs \\ xs')

removeDuplicateComplements :: Ord a => [([a], [a])] -> [([a], [a])]
removeDuplicateComplements = M.toList . foldl addComplementPair M.empty

addComplementPair :: Ord a => M.Map a a -> (a,a) -> M.Map a a
addComplementPair acc (x,y)
    | M.member y acc = acc
    | otherwise = M.insert x y acc

totalPressure :: M.Map String Int -> Int -> Network -> Int
totalPressure nodeDistances remainingTime remainingNodes = safeMaximum (M.mapWithKey totalPressure' remainingNodes)
    where   totalPressure' nextNodeName (Node nextNodePressure nextNodeDistances) = 
                let remainingTime' = remainingTime - 1 - (nodeDistances M.! nextNodeName)
                in  if remainingTime' <= 0
                    then 0
                    else
                        let pressure = remainingTime' * nextNodePressure
                            remainingNodes' = M.delete nextNodeName remainingNodes
                        in  pressure + totalPressure nextNodeDistances remainingTime' remainingNodes'

safeMaximum :: M.Map k Int -> Int
safeMaximum = safe' . M.elems
    where safe' [] = 0
          safe' xs = maximum xs
