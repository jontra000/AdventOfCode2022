module P23 (run1, run2, inputLocation) where

import qualified Data.Set as S
import Data.List (elemIndices)

type Coord = (Int, Int)
type State = S.Set Coord

data Direction = North | South | West | East

type ProposalOrders = [Direction]

inputLocation :: String
inputLocation = "inputs/input23"

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Maybe Int
run2 = solve2 . parse

parse :: String -> State
parse = S.unions . zipWith parseLine [0..] . lines

parseLine :: Int -> String -> S.Set Coord
parseLine y = S.fromList . map (coord y) . elfXCoords

elfXCoords :: String -> [Int]
elfXCoords = elemIndices '#'

coord :: Int -> Int -> Coord
coord y x = (x,y)

solve1 :: State -> Int
solve1 = emptySpacesInRectangle . (!! 10) . states

solve2 :: State -> Maybe Int
solve2 = noMoveRound . states

states :: State -> [State]
states initState = scanl doRound initState proposalOrders

noMoveRound :: [State] -> Maybe Int
noMoveRound (x:xs) = noMoveRound' 1 x xs
noMoveRound _ = Nothing

noMoveRound' :: Int -> State -> [State] -> Maybe Int
noMoveRound' _ _ [] = Nothing
noMoveRound' i a (b:xs)
    | a == b = Just i
    | otherwise = noMoveRound' (i+1) b xs

proposalOrders :: [ProposalOrders]
proposalOrders = cycle [[North,South,West,East], [South,West,East,North], [West,East,North,South], [East,North,South,West]]

doRound :: State -> ProposalOrders -> State
doRound state moveOrders = S.foldl (moveElf state moveOrders) S.empty state

moveElf :: State -> ProposalOrders -> State -> Coord -> State
moveElf oldState moveOrders newState elfCoord = moveElf' oldState moveOrders newState elfCoord neighbours'
    where neighbours' = neighbours oldState elfCoord

moveElf' :: State -> ProposalOrders -> State -> Coord -> [Bool] -> State
moveElf' _ [] newState elfCoord _ = S.insert elfCoord newState
moveElf' oldState (direction:moveOrders) newState elfCoord neighbours'
    | all (==False) neighbours' = S.insert elfCoord newState
    | hasNeighboursInDirection direction neighbours' = moveElf' oldState moveOrders newState elfCoord neighbours'
    | otherwise = handleCollision elfCoord (moveInDirection direction) newState

moveInDirection :: Direction -> Coord -> Coord
moveInDirection North (x,y) = (x, y-1)
moveInDirection South (x,y) = (x, y+1)
moveInDirection East (x,y) = (x+1, y)
moveInDirection West (x,y) = (x-1, y)

-- Uses optimisation from https://www.reddit.com/r/adventofcode/comments/zt6xz5/comment/j1dq8oj
handleCollision :: Coord -> (Coord -> Coord) -> State -> State
handleCollision elfStart delta proposedMoves =
    let elfEnd = delta elfStart
        colliderRevert = delta elfEnd
    in  if S.member elfEnd proposedMoves -- If there is already an elf here
        then S.insert elfStart $ S.insert colliderRevert $ S.delete elfEnd proposedMoves -- Our elf remains in place, other elf is moved back
        else S.insert elfEnd proposedMoves -- Otherwise, our elf moves to target

hasNeighboursInDirection :: Direction -> [Bool] -> Bool
hasNeighboursInDirection North (False:False:False:_) = False
hasNeighboursInDirection North _ = True
hasNeighboursInDirection South (_:_:_:_:False:False:False:_) = False
hasNeighboursInDirection South _ = True
hasNeighboursInDirection East (_:_:False:False:False:_) = False
hasNeighboursInDirection East _ = True
hasNeighboursInDirection West [False, _, _, _, _, _, False, False] = False
hasNeighboursInDirection West _ = True

neighbours ::State -> Coord -> [Bool]
neighbours state (x,y) = map (`S.member` state) [(x-1,y-1), (x,y-1), (x+1, y-1), (x+1,y), (x+1, y+1), (x,y+1), (x-1,y+1), (x-1,y)]

emptySpacesInRectangle :: State -> Int
emptySpacesInRectangle state = rectangleSize state - length state

rectangleSize :: State -> Int
rectangleSize state =
    let minX = minimum (S.map fst state)
        maxX = maximum (S.map fst state)
        minY = minimum (S.map snd state)
        maxY = maximum (S.map snd state)
    in  (maxX - minX + 1) * (maxY - minY + 1)