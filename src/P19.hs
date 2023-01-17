module P19 (run1, run2, inputLocation) where

import qualified Data.Map as M
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe, mapMaybe)

data Resource = Ore | Clay | Obsidian | Geode deriving (Eq, Ord, Show)

-- ore, clay, obsidian, geode
data ResourceSet = ResourceSet Int Int Int Int deriving Show
type Blueprint = M.Map Resource ResourceSet

-- blueprint, resources, robots
data State = State Blueprint ResourceSet ResourceSet deriving Show

inputLocation :: String
inputLocation = "inputs/input19"

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

parse :: String -> [Blueprint]
parse = map parseLine . lines

parseLine :: String -> Blueprint
parseLine = M.fromList . mapMaybe (parseRobot . words) . splitOn "." . dropLeadingBlueprintName

dropLeadingBlueprintName :: String -> String
dropLeadingBlueprintName = unwords . drop 2 . words

parseRobot :: [String] -> Maybe (Resource, ResourceSet)
parseRobot [] = Nothing
parseRobot (_:robotTypeStr:_:costsStrs) = Just (parseResource robotTypeStr, parseCosts costsStrs)
parseRobot e = error ("Error parsing robot string: " ++ unwords e)

parseCosts :: [String] -> ResourceSet
parseCosts = resourceSet . parseCosts'

parseCosts' :: [String] -> [(Resource, Int)]
parseCosts' [] = []
parseCosts' (_:countStr:resourceStr:xs) = (parseResource resourceStr, read countStr) : parseCosts' xs
parseCosts' e = error ("Error parsing costs string: " ++ unwords e)

parseResource :: String -> Resource
parseResource "ore" = Ore
parseResource "clay" = Clay
parseResource "obsidian" = Obsidian
parseResource "geode" = Geode
parseResource e = error ("Can't parse resource: " ++ e)

resourceSet :: [(Resource, Int)] -> ResourceSet
resourceSet xs = ResourceSet (resourceValue Ore m) (resourceValue Clay m) (resourceValue Obsidian m) (resourceValue Geode m)
    where m = M.fromList xs

resourceValue :: Resource -> M.Map Resource Int -> Int
resourceValue resource = fromMaybe 0 . M.lookup resource

solve1 :: [Blueprint] -> Int
solve1 = sum . zipWith (*) [1..] . map (geodesProduced 24)

solve2 :: [Blueprint] -> Int
solve2 = product . map (geodesProduced 32) . take 3

geodesProduced :: Int -> Blueprint -> Int
geodesProduced timeLimit blueprint = maximum (map (buildRobot timeLimit (initState blueprint)) [Ore, Clay])

initState :: Blueprint -> State
initState blueprint = State blueprint initResources (ResourceSet 1 0 0 0)

initResources :: ResourceSet
initResources = ResourceSet 0 0 0 0

removeResources :: ResourceSet -> ResourceSet -> ResourceSet
removeResources (ResourceSet baseOre baseClay baseObsidian baseGeode) (ResourceSet removeOre removeClay removeObsidian removeGeode) =
    ResourceSet (baseOre - removeOre) (baseClay - removeClay) (baseObsidian - removeObsidian) (baseGeode - removeGeode)

addResources' :: ResourceSet -> ResourceSet -> Int -> ResourceSet
addResources' (ResourceSet baseOre baseClay baseObsidian baseGeode) (ResourceSet addOre addClay addObsidian addGeode) count =
    ResourceSet (baseOre + count * addOre) (baseClay + count * addClay) (baseObsidian + count * addObsidian) (baseGeode + count * addGeode)

buildRobot :: Int -> State -> Resource -> Int
buildRobot remainingTime (State blueprint resources robots) resource
    | remainingTime' <= 0 = geodesBuilt
    | otherwise = maximum (map (buildRobot remainingTime' state') $ nextRobots state')
    where timeTaken = timeToBuildRobot remainingTime (blueprint M.! resource) resources robots
          remainingTime' = remainingTime - timeTaken
          resources' = addResources' resources robots (min remainingTime timeTaken)
          geodesBuilt = lookupResource resources' Geode
          state' = addRobot' (State blueprint resources' robots) resource

timeToBuildRobot :: Int -> ResourceSet -> ResourceSet -> ResourceSet -> Int
timeToBuildRobot remainingTime requirements resources robots = maximum (map (timeToBuildResource remainingTime requirements resources robots) [Ore, Clay, Obsidian])

timeToBuildResource :: Int -> ResourceSet -> ResourceSet -> ResourceSet -> Resource -> Int
timeToBuildResource remainingTime requirements resources robots resource
    | toBuild <= 0 = 1
    | resourceRobots <= 0 = remainingTime + 1
    | otherwise = max 0 timeToBuild + 1
    where resourceRequired = lookupResource requirements resource
          resourceInit = lookupResource resources resource
          resourceRobots = lookupResource robots resource
          toBuild = resourceRequired - resourceInit
          timeToBuild = toBuild `divRoundUp` resourceRobots

divRoundUp :: Int -> Int -> Int
divRoundUp x y = x `div` y + rounding
    where rounding = min 1 (x `mod` y)

addRobot' :: State -> Resource -> State
addRobot' (State blueprint resources robots) resource =
    let requiredResources = blueprint M.! resource
        resources' = removeResources resources requiredResources
        robots' = addRobot robots resource
    in  State blueprint resources' robots'

nextRobots :: State -> [Resource]
nextRobots state
    | canBuildRobot state Geode = [Geode]
    | canBuildRobot state Obsidian = [Obsidian]
    | otherwise = filter (shouldBuildRobot state) [Ore, Clay, Obsidian, Geode] 

canBuildRobot :: State -> Resource -> Bool
canBuildRobot (State blueprint resources _) resource =
    let required = blueprint M.! resource
        remainingResources = removeResources resources required
    in  notNegative remainingResources

shouldBuildRobot :: State -> Resource -> Bool
shouldBuildRobot _ Geode = True
shouldBuildRobot _ Obsidian = True
shouldBuildRobot (State blueprint _ robots) resource =
    let maxRequired = maximum $ map (`lookupResource` resource) (M.elems blueprint)
    in  lookupResource robots resource < maxRequired

addRobot :: ResourceSet -> Resource -> ResourceSet
addRobot (ResourceSet ore clay obsidian geode) Ore = ResourceSet (ore + 1) clay obsidian geode
addRobot (ResourceSet ore clay obsidian geode) Clay = ResourceSet ore (clay+1) obsidian geode
addRobot (ResourceSet ore clay obsidian geode) Obsidian = ResourceSet ore clay (obsidian+1) geode
addRobot (ResourceSet ore clay obsidian geode) Geode = ResourceSet ore clay obsidian (geode+1)

notNegative :: ResourceSet -> Bool
notNegative (ResourceSet ore clay obsidian geode) = ore >= 0 && clay >= 0 && obsidian >= 0 && geode >= 0

lookupResource :: ResourceSet -> Resource -> Int
lookupResource (ResourceSet ore _ _ _) Ore = ore
lookupResource (ResourceSet _ clay _ _) Clay = clay
lookupResource (ResourceSet _ _ obsidian _) Obsidian = obsidian
lookupResource (ResourceSet _ _ _ geode) Geode = geode
