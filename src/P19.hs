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
parseLine = parseBlueprint . splitOn "." . dropLeadingBlueprintName

dropLeadingBlueprintName :: String -> String
dropLeadingBlueprintName = unwords . drop 2 . words

parseBlueprint :: [String] -> Blueprint
parseBlueprint = M.fromList . mapMaybe (parseRobotCost . words)

parseRobotCost :: [String] -> Maybe (Resource, ResourceSet)
parseRobotCost [] = Nothing
parseRobotCost (_:robotTypeStr:_:costsStrs) = Just (parseResource robotTypeStr, parseCosts costsStrs)
parseRobotCost e = error ("Error parsing robot string: " ++ unwords e)

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
solve1 = sum . qualityLevels . map (geodesProduced 24)

qualityLevels :: [Int] -> [Int]
qualityLevels = zipWith (*) [1..]

solve2 :: [Blueprint] -> Int
solve2 = product . map (geodesProduced 32) . take 3

geodesProduced :: Int -> Blueprint -> Int
geodesProduced timeLimit blueprint = bestGeodes timeLimit (initState blueprint)

bestGeodes :: Int -> State -> Int
bestGeodes timeRemaining state = maximum $ map (buildNextRobot timeRemaining state) robotsToBuild
    where robotsToBuild = nextRobotsToBuild state

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

buildNextRobot :: Int -> State -> Resource -> Int
buildNextRobot remainingTime (State blueprint resources robots) resource
    | remainingTime' <= 0 = geodesBuilt
    | otherwise = bestGeodes remainingTime' state'
    where timeTaken = timeToBuildRobot remainingTime (blueprint M.! resource) resources robots
          remainingTime' = remainingTime - timeTaken
          resources' = addResources' resources robots (min remainingTime timeTaken)
          geodesBuilt = lookupResource resources' Geode
          state' = constructRobot (State blueprint resources' robots) resource

timeToBuildRobot :: Int -> ResourceSet -> ResourceSet -> ResourceSet -> Int
timeToBuildRobot remainingTime requirements resources robots = maximum (map timeToBuildResource' [Ore, Clay, Obsidian])
    where timeToBuildResource' = timeToBuildResource remainingTime requirements resources robots

timeToBuildResource :: Int -> ResourceSet -> ResourceSet -> ResourceSet -> Resource -> Int
timeToBuildResource remainingTime requirements resources robots resource
    | resourceRequired' <= 0 = 1
    | resourceRobots <= 0 = remainingTime + 1
    | otherwise = max 0 timeToBuild + 1
    where resourceRequired' = resourcesRequired requirements resources resource
          resourceRobots = lookupResource robots resource
          timeToBuild = resourceRequired' `divRoundUp` resourceRobots

resourcesRequired :: ResourceSet -> ResourceSet -> Resource -> Int
resourcesRequired requirements currentResources resource = lookupResource requirements resource - lookupResource currentResources resource

divRoundUp :: Int -> Int -> Int
divRoundUp x y = x `div` y + rounding
    where rounding = min 1 (x `mod` y)

constructRobot :: State -> Resource -> State
constructRobot (State blueprint resources robots) resource =
    let requiredResources = blueprint M.! resource
        resources' = removeResources resources requiredResources
        robots' = incrementResource robots resource
    in  State blueprint resources' robots'

nextRobotsToBuild :: State -> [Resource]
nextRobotsToBuild state
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

incrementResource :: ResourceSet -> Resource -> ResourceSet
incrementResource (ResourceSet ore clay obsidian geode) Ore = ResourceSet (ore + 1) clay obsidian geode
incrementResource (ResourceSet ore clay obsidian geode) Clay = ResourceSet ore (clay+1) obsidian geode
incrementResource (ResourceSet ore clay obsidian geode) Obsidian = ResourceSet ore clay (obsidian+1) geode
incrementResource (ResourceSet ore clay obsidian geode) Geode = ResourceSet ore clay obsidian (geode+1)

notNegative :: ResourceSet -> Bool
notNegative (ResourceSet ore clay obsidian geode) = ore >= 0 && clay >= 0 && obsidian >= 0 && geode >= 0

lookupResource :: ResourceSet -> Resource -> Int
lookupResource (ResourceSet ore _ _ _) Ore = ore
lookupResource (ResourceSet _ clay _ _) Clay = clay
lookupResource (ResourceSet _ _ obsidian _) Obsidian = obsidian
lookupResource (ResourceSet _ _ _ geode) Geode = geode
