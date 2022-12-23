module P23 (run1, run2, inputLocation) where

import qualified Data.Set as S
import Data.List (elemIndices, findIndex)

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
parseLine y = S.fromList . map (coord y . fst) . filter ((=='#') . snd) . zip [0..]

coord :: Int -> Int -> Coord
coord y x = (x,y)

solve1 :: State -> Int
solve1 state = emptySpacesInRectangle $ (!! 10) $ scanl doRound state proposalOrders

solve2 :: State -> Maybe Int
solve2 state = (+1) <$> findIndex (uncurry (==)) (zip states (tail states))
    where states = scanl doRound state proposalOrders

proposalOrders :: [ProposalOrders]
proposalOrders = cycle [[North,South,West,East], [South,West,East,North], [West,East,North,South], [East,North,South,West]]

doRound :: State -> ProposalOrders -> State
doRound state = doMoves . proposeMoves state

proposeMoves :: State -> ProposalOrders -> [(Coord, Coord)]
proposeMoves state moveOrder = S.toList $ S.map (moveElf state moveOrder) state

moveElf :: State -> ProposalOrders -> Coord -> (Coord, Coord)
moveElf state moveOrder elfCoord =
    if hasNeighbours state elfCoord
    then proposeMove state elfCoord moveOrder
    else (elfCoord, elfCoord)

proposeMove :: State -> Coord -> ProposalOrders -> (Coord, Coord)
proposeMove _ elfCoord [] = (elfCoord, elfCoord)
proposeMove state elfCoord (direction:moveOrders) =
    if hasNeighboursInDirection state direction elfCoord
    then proposeMove state elfCoord moveOrders
    else (elfCoord, moveInDirection elfCoord direction)

moveInDirection :: Coord -> Direction -> Coord
moveInDirection (x,y) North = (x, y-1)
moveInDirection (x,y) South = (x, y+1)
moveInDirection (x,y) East = (x+1, y)
moveInDirection (x,y) West = (x-1, y)

hasNeighboursInDirection :: State -> Direction -> Coord -> Bool
hasNeighboursInDirection state North (x,y) = any (`S.member` state) [(x-1,y-1), (x,y-1), (x+1, y-1)]
hasNeighboursInDirection state South (x,y) = any (`S.member` state) [(x-1,y+1), (x,y+1), (x+1, y+1)]
hasNeighboursInDirection state East (x,y) = any (`S.member` state) [(x+1,y-1), (x+1,y), (x+1, y+1)]
hasNeighboursInDirection state West (x,y) = any (`S.member` state) [(x-1,y-1), (x-1,y), (x-1, y+1)]

hasNeighbours ::State -> Coord -> Bool
hasNeighbours state (x,y) = any (`S.member` state) [(x-1,y-1), (x,y-1), (x+1, y-1), (x+1,y), (x+1, y+1), (x,y+1), (x-1,y+1), (x-1,y)]

doMoves :: [(Coord, Coord)] -> State
doMoves proposedMoves = S.fromList $ map (removeCollisions (map snd proposedMoves)) proposedMoves

removeCollisions :: [Coord] -> (Coord, Coord) -> Coord
removeCollisions proposedMoves (oldPosition, newPosition) =
    if isCollision proposedMoves newPosition
    then oldPosition
    else newPosition

isCollision :: [Coord] -> Coord -> Bool
isCollision proposedMoves position = length (elemIndices position proposedMoves) > 1

emptySpacesInRectangle :: State -> Int
emptySpacesInRectangle state = rectangleSize state - length state

rectangleSize :: State -> Int
rectangleSize state =
    let minX = minimum (S.map fst state)
        maxX = maximum (S.map fst state)
        minY = minimum (S.map snd state)
        maxY = maximum (S.map snd state)
    in  (maxX - minX + 1) * (maxY - minY + 1)