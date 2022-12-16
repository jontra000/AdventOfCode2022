module P16 (run1, run2, inputLocation) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Function (on)
import Data.List (minimumBy, delete, permutations, nub)
import Data.Maybe (mapMaybe)

-- pressure, neighbours
data Valve = Valve Int [String]
type Input = M.Map String Valve

-- pressure, distance to nodes
data Node = Node Int (M.Map String Int) deriving Show
type Network = M.Map String (M.Map String Int)
data State = State Network [(String, Int)] deriving Show

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

nodePaths :: Input -> State
nodePaths input =
    let valves = M.filter (> 0) $ M.map (\(Valve pressure _) -> pressure) input
        startingLocations = "AA" : M.keys valves
        network = M.fromSet (node input (M.keys valves)) $ S.fromList startingLocations
    in  State network (M.toList valves)

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

node :: Input -> [String] -> String -> M.Map String Int
node input destinations start = distances input (delete start destinations) start

findMaxPressure :: Int -> State -> Int
findMaxPressure timeLimit (State network valves) = maximum $ map findMaxPressure' valves
    where findMaxPressure' nextNode = totalPressure network "AA" timeLimit nextNode (delete nextNode valves)

findMaxPressure2 :: Int -> State -> Int
findMaxPressure2 timeLimit state@(State _ valves) = maximum $ map (maxPressureForPermutation timeLimit state) $ permute valves

maxPressureForPermutation :: Int -> State -> ([(String, Int)],[(String, Int)]) -> Int
maxPressureForPermutation timeLimit (State network _) (valves1, valves2) =
    findMaxPressure timeLimit (State network valves1) + findMaxPressure timeLimit (State network valves2)

permute :: [a] -> [([a], [a])]
permute xs = map listToPair $ filter ((==2) . length) $ partitions xs

partitions :: [a] -> [[[a]]]
partitions [] = [[]]
partitions (x:xs) = expand x $ partitions xs where

    expand :: a -> [[[a]]] -> [[[a]]]
    expand x ys = concatMap (extend x) ys

    extend :: a -> [[a]] -> [[[a]]]
    extend x [] = [[[x]]]
    extend x (y:ys) = ((x:y):ys) : map (y:) (extend x ys)

listToPair :: [a] -> (a,a)
listToPair (x:y:_) = (x,y)
listToPair _ = error "not enough elements to make pair"

totalPressure :: Network -> String -> Int -> (String, Int) -> [(String, Int)] -> Int
totalPressure network currentNode remainingTime (nextNode, valvePressure) remainingNodes =
    case M.lookup currentNode network of
        Nothing -> error ("Can't lookup node " ++ currentNode)
        Just valveDistances -> if remainingTime' < 0
                then 0
                else pressure + safeMaximum (map totalPressure' remainingNodes)
            where remainingTime' = remainingTime - 1 - valveDistances M.! nextNode
                  pressure = remainingTime' * valvePressure
                  totalPressure' nextNode' =
                    totalPressure network nextNode remainingTime' nextNode' (delete nextNode' remainingNodes)

safeMaximum :: [Int] -> Int
safeMaximum [] = 0
safeMaximum xs = maximum xs
