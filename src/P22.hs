module P22 (run1, run2, inputLocation) where

import qualified Data.Map as M
import Data.List.Split (splitOn)
import Data.Char (isNumber)
import Data.Maybe (catMaybes)

data Direction = Forward | TurnLeft | TurnRight
type Coord = (Int, Int)
data BoardItem = Wall | Open
type Board = M.Map Coord BoardItem
data Input = Input Board [Direction]
data Heading = FacingUp | FacingDown | FacingLeft | FacingRight
data State = State Coord Heading

inputLocation :: String
inputLocation = "inputs/input22"

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

parse :: String -> Input
parse = parse' . splitOn [""] . lines
    where parse' (boardStr:[directionsStr]:_) = Input (parseBoard boardStr) (parseDirections directionsStr)
          parse' e = error ("Error parsing input " ++ unlines (concat e))

parseDirections :: String -> [Direction]
parseDirections [] = []
parseDirections ('L':res) = TurnLeft : parseDirections res
parseDirections ('R':res) = TurnRight : parseDirections res
parseDirections s =
    let (numberStr,res) = span isNumber s
    in  replicate (read numberStr) Forward ++ parseDirections res

parseBoard :: [String] -> Board
parseBoard = M.fromList . concat . zipWith parseBoardLine [1..]

parseBoardLine :: Int -> String -> [(Coord, BoardItem)]
parseBoardLine y = catMaybes . zipWith (parseBoardItem y) [1..]

parseBoardItem :: Int -> Int -> Char -> Maybe (Coord, BoardItem)
parseBoardItem y x '.' = Just ((x,y), Open)
parseBoardItem y x '#' = Just ((x,y), Wall)
parseBoardItem _ _ _ = Nothing

solve1 :: Input -> Int
solve1 (Input board directions) = finalPassword $ foldl (applyDirection (wrappingFunction1 board) board) (initState board) directions

solve2 :: Input -> Int
solve2 (Input board directions) = finalPassword $ foldl (applyDirection (wrappingFunction2 board) board) (initState board) directions

initState :: Board -> State
initState board = State (topLeftLocation board) FacingRight

topLeftLocation :: Board -> Coord
topLeftLocation board = wrapLeft board (1,1)

applyDirection :: (State -> State) -> Board -> State -> Direction -> State
applyDirection _ _ state TurnLeft = turnLeft state
applyDirection _ _ state TurnRight = turnRight state
applyDirection wrappingFunction board state Forward = moveForward wrappingFunction board state

moveForward :: (State -> State) -> Board -> State -> State
moveForward wrappingFunction board state@(State oldLocation facing) =
    let newState = State (moveLocation oldLocation facing) facing
    in  tryMoveToLocation board state newState wrappingFunction

wrappingFunction1 :: Board -> (State -> State)
wrappingFunction1 board (State location heading) = State (wrappingFunction1' heading board location) heading

wrappingFunction1' :: Heading -> Board -> Coord -> Coord
wrappingFunction1' FacingDown = wrapDown
wrappingFunction1' FacingUp = wrapUp
wrappingFunction1' FacingLeft = wrapLeft
wrappingFunction1' FacingRight = wrapRight

wrappingFunction2 :: Board -> State -> State
wrappingFunction2 _ (State (x,_) FacingDown)
    | x <= 50 = State (x+100,1) FacingDown
    | x <= 100 = State (50, x+100) FacingLeft
    | otherwise = State (100, x-50) FacingLeft
wrappingFunction2 _ (State (x,_) FacingUp)
    | x <= 50 = State (51, x+50) FacingRight
    | x <= 100 = State (1,x+100) FacingRight
    | otherwise = State (x-100, 200) FacingUp
wrappingFunction2 _ (State (_,y) FacingRight)
    | y <= 50 = State (100, 151-y) FacingLeft
    | y <= 100 = State (y+50, 50) FacingUp
    | y <= 150 = State (150, 151-y) FacingLeft
    | otherwise = State (y-100, 150) FacingUp
wrappingFunction2 _ (State (_,y) FacingLeft)
    | y <= 50 = State (1, 151-y) FacingRight
    | y <= 100 = State (y-50, 101) FacingDown
    | y <= 150 = State (51, 151-y) FacingRight
    | otherwise = State (y-100, 1) FacingDown

wrapDown :: Board -> Coord -> Coord
wrapDown board (column,_) = minimum $ filter (\(x,_) -> x == column) $ M.keys board

wrapUp :: Board -> Coord -> Coord
wrapUp board (column,_) = maximum $ filter (\(x,_) -> x == column) $ M.keys board

wrapLeft :: Board -> Coord -> Coord
wrapLeft board (_,row) = maximum $ filter (\(_,y) -> y==row) $ M.keys board

wrapRight :: Board -> Coord -> Coord
wrapRight board (_,row) = minimum $ filter (\(_,y) -> y==row) $ M.keys board

moveLocation :: Coord -> Heading -> Coord
moveLocation (x,y) FacingDown = (x, y+1)
moveLocation (x,y) FacingUp = (x, y-1)
moveLocation (x,y) FacingLeft = (x-1, y)
moveLocation (x,y) FacingRight = (x+1, y)

tryMoveToLocation :: Board -> State -> State -> (State -> State) -> State
tryMoveToLocation board oldLocation newState@(State newLocation _) wrapFunction =
    case M.lookup newLocation board of
        Nothing -> tryMoveToLocation board oldLocation (wrapFunction newState) wrapFunction
        Just Wall -> oldLocation
        Just Open -> newState

turnRight :: State -> State
turnRight (State location facing) = State location (turnRight' facing)
    where turnRight' FacingDown = FacingLeft
          turnRight' FacingLeft = FacingUp
          turnRight' FacingUp = FacingRight
          turnRight' FacingRight = FacingDown

turnLeft :: State -> State
turnLeft (State location facing) = State location (turnLeft' facing)
    where turnLeft' FacingDown = FacingRight
          turnLeft' FacingLeft = FacingDown
          turnLeft' FacingUp = FacingLeft
          turnLeft' FacingRight = FacingUp

finalPassword :: State -> Int
finalPassword (State (x,y) facing) = 1000*y + 4*x + facingScore facing

facingScore :: Heading -> Int
facingScore FacingRight = 0
facingScore FacingDown = 1
facingScore FacingLeft = 2
facingScore FacingUp = 3