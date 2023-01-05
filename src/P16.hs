module P16 (run1, run2, inputLocation) where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (delete)

-- pressure, neighbours
data Valve = Valve Int [String]
type ValveMap = M.Map String Valve

type PressureMap = M.Map String Int
type DistanceMap = M.Map String Int
data Node = Node Int DistanceMap deriving (Eq, Ord, Show)
data Input = Input DistanceMap [(String, Node)]
type Network = M.Map String Node
type NodeList = [(String, Node)]

type UnvisitedNodes = M.Map String (Maybe Int)

inputLocation :: String
inputLocation = "inputs/input16"

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

parse :: String -> Input
parse = nodePaths . M.fromList . map (parseLine . words) . lines

parseLine :: [String] -> (String, Valve)
parseLine (_:name:_:_:pressureStr:_:_:_:_:neighboursStr) =
    let pressure = parsePressure pressureStr
        neighbours = map parseNeighbour neighboursStr
    in  (name, Valve pressure neighbours)
parseLine e = error ("Error parsing input: " ++ unwords e)

parseNeighbour :: String -> String
parseNeighbour = take 2

parsePressure :: String -> Int
parsePressure = read . drop 5 . init

solve1 :: Input -> Int
solve1 = findMaxPressure 30

solve2 :: Input -> Int
solve2 = findMaxPressure2 26

nodePaths :: ValveMap -> Input
nodePaths input =
    let valves = nonZeroValvePressures input
        valveNames = M.keys valves
    in  Input (initDistances input valveNames) (mapNodes input valves)

initDistances :: ValveMap -> [String] -> DistanceMap
initDistances input = distances input "AA"

mapNodes :: ValveMap -> PressureMap -> NodeList
mapNodes input nonZeroValves = map (node input valveNames) $ M.toList nonZeroValves
    where valveNames = M.keys nonZeroValves

nonZeroValvePressures :: ValveMap -> PressureMap
nonZeroValvePressures = M.filter (> 0) . M.map (\(Valve pressure _) -> pressure)

distances :: ValveMap -> String -> [String] -> DistanceMap
distances input start = stripNothings . mapFromKeys (distance input start)

mapFromKeys :: Ord a => (a -> b) -> [a] -> M.Map a b
mapFromKeys f = M.fromSet f . S.fromList

stripNothings :: M.Map a (Maybe b) -> M.Map a b
stripNothings = M.mapMaybe id

distance :: ValveMap -> String -> String -> Maybe Int
distance input start = dijkstra input (initUnvisitedNodes input) initialNode
    where initialNode = Just (start, 0)

initUnvisitedNodes :: ValveMap -> UnvisitedNodes
initUnvisitedNodes = M.map (const Nothing)

dijkstra :: ValveMap -> UnvisitedNodes -> Maybe (String, Int) -> String -> Maybe Int
dijkstra _ _ Nothing _ = Nothing
dijkstra valves unvisitedNodes (Just (currentNode, currentDistance)) targetNode
    | currentNode == targetNode = Just currentDistance
    | otherwise = 
        let unvisitedNodes' = updateUnvisited valves (currentDistance+1) currentNode unvisitedNodes
        in  dijkstraStep valves targetNode unvisitedNodes'

dijkstraStep :: ValveMap -> String -> UnvisitedNodes -> Maybe Int
dijkstraStep input targetNode unvisitedNodes =
    let nextNode = smallestDistance unvisitedNodes
    in  dijkstra input unvisitedNodes nextNode targetNode

smallestDistance :: UnvisitedNodes -> Maybe (String, Int)
smallestDistance = M.foldlWithKey minTentativeDistance Nothing

minTentativeDistance :: Maybe (String, Int) -> String -> Maybe Int -> Maybe (String, Int)
minTentativeDistance prev _ Nothing = prev
minTentativeDistance Nothing key (Just tentativeDistance) = Just (key, tentativeDistance)
minTentativeDistance prev@(Just (_, prevDistance)) key (Just tentativeDistance)
    | tentativeDistance < prevDistance = Just (key, tentativeDistance)
    | otherwise = prev

updateUnvisited :: ValveMap -> Int -> String -> UnvisitedNodes -> UnvisitedNodes
updateUnvisited valves nextDistance currentNode unvisitedNodes = M.delete currentNode $ updateNeighbours valves nextDistance unvisitedNodes currentNode

updateNeighbours :: ValveMap -> Int -> UnvisitedNodes -> String -> UnvisitedNodes
updateNeighbours valves currentDistance unvisitedNodes = foldl (updateNeighbour currentDistance) unvisitedNodes . reachableNeighbours valves

reachableNeighbours :: ValveMap -> String -> [String]
reachableNeighbours input nodeName = case input M.! nodeName of (Valve _ neighbours) -> neighbours

updateNeighbour :: Int -> UnvisitedNodes -> String -> UnvisitedNodes
updateNeighbour currentDistance unvisitedNodes key =
    M.adjust (updateDistance currentDistance) key unvisitedNodes

updateDistance :: Int -> Maybe Int -> Maybe Int
updateDistance d Nothing = Just d
updateDistance dNew (Just dOld) = Just (min dNew dOld)

node :: ValveMap -> [String] -> (String, Int) -> (String, Node)
node input destinations (start, pressure) = (start, Node pressure $ distances input start (delete start destinations))

findMaxPressure :: Int -> Input -> Int
findMaxPressure _ (Input _ []) = 0
findMaxPressure timeLimit (Input distanceMap nodes) = runOutputs1 timeLimit distanceMap $ M.fromList nodes

findMaxPressure2 :: Int -> Input -> Int
findMaxPressure2 timeLimit (Input distanceMap network) =
    let humanResults = maximumPressures $ runOutputs2 timeLimit distanceMap $ M.fromList network
        bestSingleRun = maximum (map snd humanResults)
    in  foldl (runElephants timeLimit distanceMap network bestSingleRun) 0 humanResults

runElephants :: Int -> DistanceMap -> NodeList -> Int -> Int -> ([String], Int) -> Int
runElephants timeLimit distanceMap network bestSingleRun bestDoubleRun (availableNodes, runResult)
    | runResult + bestSingleRun > bestDoubleRun =
        let nextResult = elephantResults timeLimit distanceMap (filterNetwork (M.fromList network) availableNodes) runResult
        in  max nextResult bestDoubleRun
    | otherwise = bestDoubleRun

filterNetwork :: Network -> [String] -> Network
filterNetwork network = M.restrictKeys network . S.fromList

elephantResults :: Int -> DistanceMap -> Network -> Int -> Int
elephantResults timeLimit distanceMap unvisitedNodes humansPressure = humansPressure + runOutputs1 timeLimit distanceMap unvisitedNodes

runOutputs1 :: Int -> DistanceMap -> Network -> Int
runOutputs1 remainingTime distanceMap network = maximum $ map (moveToNode1 distanceMap remainingTime network) $ M.toList network

runOutputs2 :: Int -> DistanceMap -> Network -> [M.Map [String] Int]
runOutputs2 remainingTime distanceMap network = concatMap (moveToNode2 distanceMap remainingTime network) $ M.toList network

maximumPressures :: [M.Map [String] Int] -> [([String], Int)]
maximumPressures = M.toList . M.unionsWith max

moveToNode2 :: DistanceMap -> Int -> Network -> (String, Node) -> [M.Map [String] Int]
moveToNode2 nodeDistances remainingTime remainingNodes (nextNodeName, Node nextNodePressure nextNodeDistances)
    | remainingTime' <= 0 = [M.singleton (M.keys remainingNodes) 0]
    | otherwise = map (M.map (+ pressure)) $ runOutputs2 remainingTime' nextNodeDistances remainingNodes'
        where   remainingTime' = decreaseRemainingTime remainingTime nodeDistances nextNodeName
                pressure = remainingTime' * nextNodePressure
                remainingNodes' = M.delete nextNodeName remainingNodes

moveToNode1 :: DistanceMap -> Int -> Network -> (String, Node) -> Int
moveToNode1 nodeDistances remainingTime remainingNodes (nextNodeName, Node nextNodePressure nextNodeDistances)
    | remainingTime' <= 0 = 0
    | otherwise = pressure + runOutputs1 remainingTime' nextNodeDistances remainingNodes'
        where   remainingTime' = decreaseRemainingTime remainingTime nodeDistances nextNodeName
                pressure = remainingTime' * nextNodePressure
                remainingNodes' = M.delete nextNodeName remainingNodes

decreaseRemainingTime :: Int -> DistanceMap -> String -> Int
decreaseRemainingTime remainingTime nodeDistances nextNodeName = remainingTime - 1 - (nodeDistances M.! nextNodeName)