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
parseLine = M.fromList . map (parseRobot . words) . filter (not . null) . splitOn "." . unwords . drop 2 . words

parseRobot :: [String] -> (Resource, ResourceSet)
parseRobot (_:robotTypeStr:_:costsStrs) = (parseResource robotTypeStr, parseCosts costsStrs)
parseRobot e = error ("Error parsing robot string: " ++ unwords e)

parseCosts :: [String] -> ResourceSet
parseCosts = resourceSet . M.fromList . parseCosts'

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

resourceSet :: M.Map Resource Int -> ResourceSet
resourceSet m = ResourceSet (fromMaybe 0 (M.lookup Ore m)) (fromMaybe 0 (M.lookup Clay m)) (fromMaybe 0 (M.lookup Obsidian m)) (fromMaybe 0 (M.lookup Geode m))

solve1 :: [Blueprint] -> Int
solve1 = sum . zipWith (*) [1..] . map (geodesProduced 24)

solve2 :: [Blueprint] -> Int
solve2 = product . map (geodesProduced 32) . take 3

geodesProduced :: Int -> Blueprint -> Int
geodesProduced timeLimit = runFactory timeLimit . initState

initState :: Blueprint -> State
initState blueprint = State blueprint initResources (ResourceSet 1 0 0 0)

initResources :: ResourceSet
initResources = ResourceSet 0 0 0 0

runFactory :: Int -> State -> Int
runFactory 0 state = countGeodes state
runFactory i state@(State _ _ robots) =
    maximum $ map (runFactory (i-1) . buildResources robots) $ buildRobots state

buildResources :: ResourceSet -> State -> State
buildResources robots (State blueprint resources robots') = State blueprint (addResources robots resources) robots'

removeResources :: ResourceSet -> ResourceSet -> ResourceSet
removeResources (ResourceSet baseOre baseClay baseObsidian baseGeode) (ResourceSet removeOre removeClay removeObsidian removeGeode) =
    ResourceSet (baseOre - removeOre) (baseClay - removeClay) (baseObsidian - removeObsidian) (baseGeode - removeGeode)

addResources :: ResourceSet -> ResourceSet -> ResourceSet
addResources (ResourceSet baseOre baseClay baseObsidian baseGeode) (ResourceSet addOre addClay addObsidian addGeode) =
    ResourceSet (baseOre + addOre) (baseClay + addClay) (baseObsidian + addObsidian) (baseGeode + addGeode)

buildRobots :: State -> [State]
buildRobots state =
    case tryBuildRobot state Geode of
        Just geodeBot -> [geodeBot]
        Nothing -> case tryBuildRobot state Obsidian of
            Just obsidianBot -> [obsidianBot]
            Nothing -> state : mapMaybe (tryBuildRobot state) (filter (shouldBuildRobot state) [Ore, Clay])

tryBuildRobot :: State -> Resource -> Maybe State
tryBuildRobot (State blueprint resources robots) resource =
    let required = blueprint M.! resource
        remainingResources = removeResources resources required
    in  if notNegative remainingResources
        then Just (State blueprint remainingResources (addRobot robots resource))
        else Nothing

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

countGeodes :: State -> Int
countGeodes (State _ (ResourceSet _ _ _ geodes) _) = geodes

lookupResource :: ResourceSet -> Resource -> Int
lookupResource (ResourceSet ore _ _ _) Ore = ore
lookupResource (ResourceSet _ clay _ _) Clay = clay
lookupResource (ResourceSet _ _ obsidian _) Obsidian = obsidian
lookupResource (ResourceSet _ _ _ geode) Geode = geode
