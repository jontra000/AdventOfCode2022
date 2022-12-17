module P17 (run1, run2, inputLocation) where

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.List (find)

data JetDirection = JetLeft | JetRight deriving (Eq, Show)

type Coord = (Int, Int)
type Rock = S.Set Coord

-- top 10 row state, jet index, rock index
type State2 = (Rock, Int, Int)
type StateCache = M.Map State2 (Int, Int)

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
solve rockCount jetDirections = iterate2 (length jetDirections) M.empty rockCount (S.fromList (map (`pair` 0) [0..6])) (setYCoord (head rocks) 4) 0 (tail rocks ++ cycle rocks) (cycle jetDirections) 0 1

pair :: Int -> Int -> Coord
pair x y= (x,y)

rocks :: [Rock]
rocks = [S.fromList [(2,0),(3,0),(4,0),(5,0)], S.fromList [(2,1), (3,0), (3,1), (3,2), (4,1)], S.fromList [(2,0),(3,0),(4,0),(4,1),(4,2)], S.fromList [(2,0),(2,1),(2,2),(2,3)], S.fromList [(2,0),(2,1),(3,0),(3,1)]]

dropRock1 :: Rock -> Rock -> Rock
dropRock1 = moveRock down

blowRock :: JetDirection -> Rock -> Rock -> Rock
blowRock JetLeft = moveRock left
blowRock JetRight = moveRock right

moveRock :: (Coord -> Coord) -> Rock -> Rock -> Rock
moveRock dir state rock =
    let rock' = S.map dir rock
    in  if inBounds rock' && S.disjoint state rock'
        then rock'
        else rock

inBounds :: Rock -> Bool
inBounds = all inBounds'

inBounds' :: Coord -> Bool
inBounds' (x,_) = x >= 0 && x < 7

down :: Coord -> Coord
down (x,y) = (x, y-1)

left :: Coord -> Coord
left (x,y) = (x-1, y)

right :: Coord -> Coord
right (x,y) = (x+1, y)

setYCoord :: Rock -> Int -> Rock
setYCoord rock y = S.map (setYCoord' y) rock

setYCoord' :: Int -> Coord -> Coord
setYCoord' h (x,y) = (x, y+h)

purgeLowRocks :: Int -> Rock -> Rock
purgeLowRocks height = S.filter (overHeight (height - 50))

overHeight :: Int -> Coord -> Bool
overHeight h (_,y)= y > h

iterate2 :: Int -> StateCache -> Int -> Rock -> Rock -> Int -> [Rock] -> [JetDirection] -> Int -> Int -> Int
iterate2 _ _ _ _ _ _ _ [] _ _ = error "End of infinite list"
iterate2 jetLength stateCache targetRock stoppedRocks currentRock height nextRocks (jet:jets) jetCount currentRockCount =
    let rock' = blowRock jet stoppedRocks currentRock
        rock'' = dropRock1 stoppedRocks rock'
        stoppedRocks' = purgeLowRocks height $ S.union stoppedRocks rock''
        height' = max height $ maximum $ S.toList (S.map snd rock'')
        jetCount' = (jetCount + 1) `mod` jetLength
    in  if rock' == rock''
        then stopRock jetLength stateCache targetRock stoppedRocks' height' nextRocks jets jetCount' currentRockCount
        else iterate2 jetLength stateCache targetRock stoppedRocks rock'' height nextRocks jets jetCount' currentRockCount

stopRock :: Int -> StateCache -> Int -> Rock -> Int -> [Rock] -> [JetDirection] -> Int -> Int -> Int
stopRock _ _ _ _ _ [] _ _ _ = error "End of infinite rocks list"
stopRock jetLength stateCache targetRock stoppedRocks height (nextRock:rocks) jets jetCount currentRockCount
    | currentRockCount == targetRock = height
    | otherwise =
        let state = (sampleRockTop height stoppedRocks, jetCount, currentRockCount `mod` 5)
            stateCache' = M.insert state (currentRockCount, height) stateCache
        in  case checkRepeatState state stateCache of
                Just (cycleStart, heightStart) -> repeatState stateCache targetRock height cycleStart heightStart currentRockCount
                Nothing -> iterate2 jetLength stateCache' targetRock stoppedRocks (setYCoord nextRock (height + 4)) height rocks jets jetCount (currentRockCount + 1)

repeatState :: StateCache -> Int -> Int -> Int -> Int -> Int -> Int
repeatState cache targetRock height cycleStart startHeight cycleEnd =
    let rockCountDelta = cycleEnd - cycleStart
        iterations = (targetRock - cycleStart) `div` rockCountDelta
        height' = iterations * (height - startHeight)
        remainingCyclePoint = targetRock - iterations * rockCountDelta
    -- in  error (show cycleStart ++ " " ++ show cycleEnd ++ " " ++ show rockCountDelta ++ " " ++ show iterations ++ " " ++ show height' ++ " " ++ show remainingCyclePoint
    --             ++ " " ++ show startHeight ++ " " ++ show height)
    in  case fmap snd $ find ((== remainingCyclePoint) . fst) $ M.elems cache of
            Nothing -> error "can't look up cycle value"
            Just additionalHeight -> height' + additionalHeight

checkRepeatState :: State2 -> StateCache -> Maybe (Int, Int)
checkRepeatState = M.lookup

sampleRockTop :: Int -> Rock -> Rock
sampleRockTop height rock = S.filter (\(_,y) -> y > 0) $ S.map (\(x,y) -> (x, y - height + 17)) rock

-- 1592369438111