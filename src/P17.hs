module P17 (run1, run2, inputLocation) where

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.List (find)

data JetDirection = JetLeft | JetRight deriving (Eq, Show)

type Coord = (Int, Int)
type Rock = S.Set Coord

-- top 10 row state, jet index, rock index
type State = (Rock, Int, Int)
-- Map state to (rockCount, height)
type StateCache = M.Map State (Int, Int)

inputLocation :: String
inputLocation = "inputs/input17"

run1 :: String -> Int
run1 = solve 2022 . parse

run2 :: String -> Int
run2 = solve 1000000000000 . parse

parse :: String -> [JetDirection]
parse = map parseJetChar

parseJetChar :: Char -> JetDirection
parseJetChar '<' = JetLeft
parseJetChar '>' = JetRight
parseJetChar c = error ("bad jet char: " ++ [c])

solve :: Int -> [JetDirection] -> Int
solve rockCount jetDirections = iterateRock jetCycleLength M.empty rockCount rockFloor initialRock 0 rockCycle jetCycle 0 1
    where jetCycleLength = length jetDirections
          rockFloor = S.fromList (map (`pair` 0) [0..6])
          initialRock = setRockHeight 4 (head rocks)
          rockCycle = tail rocks ++ cycle rocks
          jetCycle = cycle jetDirections

pair :: Int -> Int -> Coord
pair x y= (x,y)

rocks :: [Rock]
rocks = [S.fromList [(2,0),(3,0),(4,0),(5,0)], S.fromList [(2,1), (3,0), (3,1), (3,2), (4,1)], S.fromList [(2,0),(3,0),(4,0),(4,1),(4,2)],
            S.fromList [(2,0),(2,1),(2,2),(2,3)], S.fromList [(2,0),(2,1),(3,0),(3,1)]]

dropRock1 :: Rock -> Rock -> Rock
dropRock1 = moveRock down

blowRock :: JetDirection -> Rock -> Rock -> Rock
blowRock JetLeft = moveRock left
blowRock JetRight = moveRock right

moveRock :: (Coord -> Coord) -> Rock -> Rock -> Rock
moveRock dir state rock
    | collision state rock' = rock
    | inBounds rock' = rock'
    | otherwise = rock
        where rock' = S.map dir rock

inBounds :: Rock -> Bool
inBounds = all inBounds'

inBounds' :: Coord -> Bool
inBounds' (x,_) = x >= 0 && x < 7

collision :: Rock -> Rock -> Bool
collision a b = not $ S.disjoint a b

down :: Coord -> Coord
down (x,y) = (x, y-1)

left :: Coord -> Coord
left (x,y) = (x-1, y)

right :: Coord -> Coord
right (x,y) = (x+1, y)

setRockHeight :: Int -> Rock -> Rock
setRockHeight y = S.map (setYCoord y)

setYCoord :: Int -> Coord -> Coord
setYCoord h (x,y) = (x, y+h)

iterateRock :: Int -> StateCache -> Int -> Rock -> Rock -> Int -> [Rock] -> [JetDirection] -> Int -> Int -> Int
iterateRock _ _ _ _ _ _ _ [] _ _ = error "End of infinite jets list"
iterateRock jetLength stateCache targetRockCount stoppedRocks currentRock height nextRocks (jet:jets) jetCount currentRockCount =
    let rock' = blowRock jet stoppedRocks currentRock
        rock'' = dropRock1 stoppedRocks rock'
        stoppedRocks' = S.union stoppedRocks rock''
        height' = maxHeight height rock''
        jetCount' = jetCount + 1
    in  if rock' == rock''
        then stopRock jetLength stateCache targetRockCount stoppedRocks' height' nextRocks jets jetCount' currentRockCount
        else iterateRock jetLength stateCache targetRockCount stoppedRocks rock'' height nextRocks jets jetCount' currentRockCount

maxHeight :: Int -> Rock -> Int
maxHeight oldHeight = max oldHeight . maximum . S.toList . S.map snd

stopRock :: Int -> StateCache -> Int -> Rock -> Int -> [Rock] -> [JetDirection] -> Int -> Int -> Int
stopRock _ _ _ _ _ [] _ _ _ = error "End of infinite rocks list"
stopRock jetLength stateCache targetRockCount stoppedRocks height (nextRock:rockCycle) jets jetCount currentRockCount
    | currentRockCount == targetRockCount = height
    | otherwise =
        let jetCount' = jetCount `mod` jetLength
            state = (sampleRockTop height stoppedRocks, jetCount', currentRockCount `mod` 5)
            stateCache' = M.insert state (currentRockCount, height) stateCache
            nextRockAtTop = setRockHeight (height + 4) nextRock
        in  case checkRepeatState state stateCache of
                Just (cycleStart, heightStart) -> repeatState stateCache targetRockCount height cycleStart heightStart currentRockCount
                Nothing -> iterateRock jetLength stateCache' targetRockCount stoppedRocks nextRockAtTop height rockCycle jets jetCount' (currentRockCount + 1)

repeatState :: StateCache -> Int -> Int -> Int -> Int -> Int -> Int
repeatState cache targetRockCount height cycleStart startHeight cycleEnd =
    let cycleLength = cycleEnd - cycleStart
        cycleIterations = (targetRockCount - cycleStart) `div` cycleLength
        cycleHeight = height - startHeight
        fastForwardHeight = cycleIterations * cycleHeight
        rockCountRemainder = targetRockCount `mod` cycleLength
    in  case lookupHeight rockCountRemainder cache of
            Nothing -> error "rock count value not stored in cache"
            Just additionalHeight -> fastForwardHeight + additionalHeight

lookupHeight :: Int -> StateCache -> Maybe Int
lookupHeight rockCount = fmap snd . find ((== rockCount) . fst) . M.elems

checkRepeatState :: State -> StateCache -> Maybe (Int, Int)
checkRepeatState = M.lookup

sampleRockTop :: Int -> Rock -> Rock
sampleRockTop height = pruneRock . shiftToHeight height 17

shiftToHeight :: Int -> Int -> Rock -> Rock
shiftToHeight oldHeight newHeight = S.map (\(x,y) -> (x, y - oldHeight + newHeight))

pruneRock :: Rock -> Rock
pruneRock = S.filter (\(_,y) -> y > 0)