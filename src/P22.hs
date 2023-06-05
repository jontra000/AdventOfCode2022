module P22 (run1, run2, inputLocation) where

import qualified Data.Map as M
import Data.List.Split (splitOn)
import Data.Char (isNumber)
import Data.Maybe (catMaybes)
import Data.Tuple (swap)
import Data.List (delete)

data Direction = Forward | TurnLeft | TurnRight
type Coord = (Int, Int)
data BoardItem = Wall | Open deriving Show
type Board = M.Map Coord BoardItem
type Faces = M.Map CubeOrientation CubeFace
data Cube = Cube Int Faces deriving Show
data Input = Input Board [Direction]
data Heading = FacingUp | FacingDown | FacingLeft | FacingRight deriving (Eq, Show)
data State = StateFlat Board Coord Heading | StateCube Cube CubeOrientation Coord Heading deriving Show
data CubeOrientation = Front | LeftFace | RightFace | Top | Bottom | Back deriving (Eq, Ord, Show)
data Transform = None | RotateLeft | RotateRight | Rotate180 deriving Show
data CubeFace = CubeFace Board Coord Transform deriving Show

-- Cube face orientation relations (same pattern for all faces):
--
--   ---
--   ->-
-- -------
-- -v-^->-
-- -------
--   -v-
--   ---

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
solve1 (Input board directions) = solve directions (initStateFlat board)

solve2 :: Input -> Int
solve2 (Input board directions) = solve directions (initStateCube board)

solve :: [Direction] -> State -> Int
solve directions state = finalPassword $ foldl applyDirection state directions

initStateFlat :: Board -> State
initStateFlat board = StateFlat board (topLeftLocation board) FacingRight

initStateCube :: Board -> State
initStateCube board = StateCube (makeCube board) Front (1,1) FacingRight

topLeftLocation :: Board -> Coord
topLeftLocation board = wrapLeft board (1,1)

applyDirection :: State -> Direction -> State
applyDirection state TurnLeft = turnLeft state
applyDirection state TurnRight = turnRight state
applyDirection state Forward = tryMoveForward state

tryMoveForward :: State -> State
tryMoveForward state =
    let state' = moveForward state
    in  case currentTile state' of
            Wall -> state
            _ -> state'

moveForward :: State -> State
moveForward (StateFlat board oldLocation heading) =
    let newLocation = moveForward' oldLocation heading
    in  if M.member newLocation board then StateFlat board newLocation heading else wrapFlat board newLocation heading
moveForward (StateCube cube@(Cube _ faces) orientation oldLocation heading) =
    let newLocation = moveForward' oldLocation heading
        CubeFace board _ _ = faces M.! orientation
    in  if M.member newLocation board then StateCube cube orientation newLocation heading else wrapCube cube orientation newLocation heading

wrapFlat :: Board -> Coord -> Heading -> State
wrapFlat board location heading = StateFlat board (wrapFlat' heading board location) heading

wrapFlat' :: Heading -> Board -> Coord -> Coord
wrapFlat' FacingDown = wrapDown
wrapFlat' FacingUp = wrapUp
wrapFlat' FacingLeft = wrapLeft
wrapFlat' FacingRight = wrapRight

wrapCube :: Cube -> CubeOrientation -> Coord -> Heading -> State
wrapCube cube@(Cube sideLength _) orientation location heading =
    let orientation' = moveFace orientation heading
        heading' = wrapHeadingCube heading
        location' = wrapLocationCube sideLength heading location
    in  StateCube cube orientation' location' heading'

moveFace :: CubeOrientation -> Heading -> CubeOrientation
moveFace Front FacingUp = Top
moveFace Front FacingLeft = LeftFace
moveFace Front FacingRight = RightFace
moveFace Front FacingDown = Bottom
moveFace Bottom FacingUp = Back
moveFace Bottom FacingLeft = RightFace
moveFace Bottom FacingRight = LeftFace
moveFace Bottom FacingDown = Front
moveFace Top FacingUp = RightFace
moveFace Top FacingLeft = Back
moveFace Top FacingRight = Front
moveFace Top FacingDown = LeftFace
moveFace LeftFace FacingUp = Bottom
moveFace LeftFace FacingLeft = Front
moveFace LeftFace FacingRight = Back
moveFace LeftFace FacingDown = Top
moveFace RightFace FacingUp = Front
moveFace RightFace FacingLeft = Bottom
moveFace RightFace FacingRight = Top
moveFace RightFace FacingDown = Back
moveFace Back FacingUp = LeftFace
moveFace Back FacingLeft = Top
moveFace Back FacingRight = Bottom
moveFace Back FacingDown = RightFace

wrapHeadingCube :: Heading -> Heading
wrapHeadingCube FacingUp = FacingLeft
wrapHeadingCube FacingLeft = FacingRight
wrapHeadingCube FacingRight = FacingDown
wrapHeadingCube FacingDown = FacingUp

wrapLocationCube :: Int -> Heading -> Coord -> Coord
wrapLocationCube sideLength FacingUp (x,_) = (sideLength, sideLength + 1 - x)
wrapLocationCube sideLength FacingLeft (_,y) = (1, sideLength + 1 - y)
wrapLocationCube sideLength FacingRight (_,y) = (sideLength + 1 - y, 1)
wrapLocationCube sideLength FacingDown (x,_) = (sideLength + 1 - x, sideLength)

wrapDown :: Board -> Coord -> Coord
wrapDown board (column,_) = minimum $ filter (\(x,_) -> x == column) $ M.keys board

wrapUp :: Board -> Coord -> Coord
wrapUp board (column,_) = maximum $ filter (\(x,_) -> x == column) $ M.keys board

wrapLeft :: Board -> Coord -> Coord
wrapLeft board (_,row) = maximum $ filter (\(_,y) -> y==row) $ M.keys board

wrapRight :: Board -> Coord -> Coord
wrapRight board (_,row) = minimum $ filter (\(_,y) -> y==row) $ M.keys board

moveForward' :: Coord -> Heading -> Coord
moveForward' (x,y) FacingDown = (x, y+1)
moveForward' (x,y) FacingUp = (x, y-1)
moveForward' (x,y) FacingLeft = (x-1, y)
moveForward' (x,y) FacingRight = (x+1, y)

turnRight :: State -> State
turnRight (StateFlat board location facing) = StateFlat board location (turnRight' facing)
turnRight (StateCube cube orientation location facing) = StateCube cube orientation location (turnRight' facing)

turnRight' :: Heading -> Heading
turnRight' FacingDown = FacingLeft
turnRight' FacingLeft = FacingUp
turnRight' FacingUp = FacingRight
turnRight' FacingRight = FacingDown

turnLeft :: State -> State
turnLeft (StateFlat board location facing) = StateFlat board location (turnLeft' facing)
turnLeft (StateCube cube orientation location facing) = StateCube cube orientation location (turnLeft' facing)

turnLeft' :: Heading -> Heading
turnLeft' FacingDown = FacingRight
turnLeft' FacingLeft = FacingDown
turnLeft' FacingUp = FacingLeft
turnLeft' FacingRight = FacingUp

finalPassword :: State -> Int
finalPassword (StateFlat _ c facing) = finalPassword' c facing
finalPassword (StateCube cube orientation c facing) =
    let (boardCoords, boardFacing) = cubeLocationToFlatLocation cube orientation c facing
    in  finalPassword' boardCoords boardFacing

finalPassword' :: Coord -> Heading -> Int
finalPassword' (x,y) facing = 1000*y + 4*x + facingScore facing

cubeLocationToFlatLocation :: Cube -> CubeOrientation -> Coord -> Heading -> (Coord, Heading)
cubeLocationToFlatLocation (Cube sideLength cube) orientation c facing =
    let cubeFace = cube M.! orientation
        c' = translateCubeCoords sideLength cubeFace c
        facing' = translateCubeFacing cubeFace facing
    in  (c', facing')

translateCubeCoords :: Int -> CubeFace -> Coord -> Coord
translateCubeCoords sideLength (CubeFace _ (boardX, boardY) translation) c =
    let (x', y') = translateCubeCoords' sideLength (reverseTranslation translation) c
    in  (boardX + x' - 1, boardY + y' - 1)

reverseTranslation :: Transform -> Transform
reverseTranslation RotateLeft = RotateRight
reverseTranslation RotateRight = RotateLeft
reverseTranslation x = x

translateCubeCoords' :: Int -> Transform -> Coord -> Coord
translateCubeCoords' _ None c = c
translateCubeCoords' sideLength RotateLeft (x, y) = (sideLength + 1 - y, x)
translateCubeCoords' sideLength RotateRight (x, y) = (y, sideLength + 1- x)
translateCubeCoords' sideLength Rotate180 (x, y) = (sideLength + 1 - x, sideLength + 1 - y)

translateCubeFacing :: CubeFace -> Heading -> Heading
translateCubeFacing (CubeFace _ _ None) h = h
translateCubeFacing (CubeFace _ _ RotateLeft) FacingUp = FacingLeft
translateCubeFacing (CubeFace _ _ RotateLeft) FacingRight = FacingUp
translateCubeFacing (CubeFace _ _ RotateLeft) FacingDown = FacingRight
translateCubeFacing (CubeFace _ _ RotateLeft) FacingLeft = FacingDown
translateCubeFacing (CubeFace _ _ RotateRight) FacingUp = FacingRight
translateCubeFacing (CubeFace _ _ RotateRight) FacingLeft = FacingUp
translateCubeFacing (CubeFace _ _ RotateRight) FacingDown = FacingLeft
translateCubeFacing (CubeFace _ _ RotateRight) FacingRight = FacingDown
translateCubeFacing (CubeFace _ _ Rotate180) FacingUp = FacingDown
translateCubeFacing (CubeFace _ _ Rotate180) FacingLeft = FacingRight
translateCubeFacing (CubeFace _ _ Rotate180) FacingDown = FacingUp
translateCubeFacing (CubeFace _ _ Rotate180) FacingRight = FacingLeft

facingScore :: Heading -> Int
facingScore FacingRight = 0
facingScore FacingDown = 1
facingScore FacingLeft = 2
facingScore FacingUp = 3

makeCube :: Board -> Cube
makeCube board = Cube faceLength' $ constructCube faceLength' board $ faceLocations faceLength' board
    where faceLength' = faceLength board

faceLength :: Board -> Int
faceLength board =
    let coords = M.keys board
        maxX = maximum $ map fst coords
        maxY = maximum $ map snd coords
        result = max maxX maxY `div` 4
    in  if maxX `mod` result == 0 && maxY `mod` result == 0 && (maxY `div` 3 == result || maxX `div` 3 == result)
        then result
        else error "Input not 3x4 grid"

faceLocations :: Int -> Board -> [Coord]
faceLocations gridSize board = filter (`M.member` board) [(x,y) | x <- gridPoints, y <- gridPoints]
    where gridPoints = map (+1) [0, gridSize, gridSize*2, gridSize*3]

constructCube :: Int -> Board -> [Coord] -> Faces
constructCube sideLength board gridLocations =
    let startLocation' = startLocation gridLocations
        firstFace = gridFaceFront sideLength board startLocation'
    in  addCubeNeighbours board sideLength (delete startLocation' gridLocations) Front firstFace (M.singleton Front firstFace) 

startLocation :: [Coord] -> Coord
startLocation = swap . minimum . map swap

gridFaceFront :: Int -> Board -> Coord -> CubeFace
gridFaceFront sideLength board c = CubeFace boardFace c None
    where boardFace = subBoard sideLength c board

addCubeNeighbours :: Board -> Int -> [Coord] -> CubeOrientation -> CubeFace -> Faces -> Faces
addCubeNeighbours board gridSize remainingLocations currentFace face@(CubeFace _ currentLocation _) currentCube =
    let neighbours = neighbouringFaces gridSize remainingLocations currentLocation
    in  foldl (addFace board gridSize remainingLocations currentFace face) currentCube neighbours

neighbouringFaces :: Int -> [Coord] -> Coord -> [Coord]
neighbouringFaces gridSize remainingLocations currentLocation =
    filter (isNeighbour gridSize currentLocation) remainingLocations

isNeighbour :: Int -> Coord -> Coord -> Bool
isNeighbour sideLength (x1, y1) (x2, y2)
    | (x1 == x2) && dy == sideLength = True
    | (y1 == y2) && dx == sideLength = True
    | otherwise = False
        where dx = abs (x1 - x2)
              dy = abs (y1 - y2)

addFace :: Board -> Int -> [Coord] -> CubeOrientation -> CubeFace -> Faces -> Coord -> Faces
addFace board gridSize remainingLocations connectingOrientation (CubeFace _ connectingLocation connectingTransform) cube currentLocation =
    let heading = neighbourHeading connectingTransform connectingLocation currentLocation
        orientation = neighbourOrientation connectingOrientation heading
        transform = neighbourTransform connectingTransform heading
        faceBoard' = faceBoard gridSize transform currentLocation board
        face = CubeFace faceBoard' currentLocation transform
        cube' = M.insert orientation face cube
    in  addCubeNeighbours board gridSize (delete currentLocation remainingLocations) orientation face cube'

faceBoard :: Int -> Transform -> Coord -> Board -> Board
faceBoard sideLength transform c board = transformBoard sideLength transform $ subBoard sideLength c board

subBoard :: Int -> Coord -> Board -> Board
subBoard sideLength c = validateSubBoard sideLength c . translateSubBoard c . filterSubBoard sideLength c

filterSubBoard :: Int -> Coord -> Board -> Board
filterSubBoard sideLength (x,y) = M.filterWithKey (\(x', y') _ -> x' >= x && x' < x + sideLength && y' >= y && y' < y + sideLength)

translateSubBoard :: Coord -> Board -> Board
translateSubBoard (x,y) = M.mapKeys (\(x', y') -> (x' - x + 1, y' - y + 1))

validateSubBoard :: Int -> Coord -> Board -> Board
validateSubBoard sideLength c board = 
    if length board == sideLength * sideLength then board else error ("Bad board size at " ++ show c ++ " " ++ show board)

transformBoard :: Int -> Transform -> Board -> Board
transformBoard cubeLength transform = M.mapKeys (translateCubeCoords' cubeLength transform)

neighbourOrientation :: CubeOrientation -> Heading -> CubeOrientation
neighbourOrientation Front = neighbourOrientationFront
neighbourOrientation Top = neighbourOrientationTop
neighbourOrientation LeftFace = neighbourOrientationLeft
neighbourOrientation RightFace = neighbourOrientationRight
neighbourOrientation Bottom = neighbourOrientationBottom
neighbourOrientation Back = neighbourOrientationBack

neighbourHeading :: Transform -> Coord -> Coord -> Heading
neighbourHeading transform cPrev = applyTransform transform . neighbourHeading' cPrev

neighbourHeading' :: Coord -> Coord -> Heading
neighbourHeading' cPrev@(connectingX, connectingY) cNext@(x, y)
    | connectingX == x && connectingY > y = FacingUp
    | connectingX == x && connectingY < y = FacingDown
    | connectingY == y && connectingX > x = FacingLeft
    | connectingY == y && connectingX < x = FacingRight
    | otherwise = error ("non-aligned face coordinates: " ++ show cPrev ++ " and " ++ show cNext)

neighbourTransform :: Transform -> Heading -> Transform
neighbourTransform transform = combineTransforms transform . neighbourTransform'

neighbourTransform' :: Heading -> Transform
neighbourTransform' FacingUp = RotateRight
neighbourTransform' FacingRight = RotateLeft
neighbourTransform' FacingDown = Rotate180
neighbourTransform' FacingLeft = Rotate180

applyTransform :: Transform -> Heading -> Heading
applyTransform None x = x
applyTransform RotateLeft FacingUp = FacingRight
applyTransform RotateLeft FacingLeft = FacingUp
applyTransform RotateLeft FacingDown = FacingLeft
applyTransform RotateLeft FacingRight = FacingDown
applyTransform RotateRight FacingUp = FacingLeft
applyTransform RotateRight FacingRight = FacingUp
applyTransform RotateRight FacingDown = FacingRight
applyTransform RotateRight FacingLeft = FacingDown
applyTransform Rotate180 FacingUp = FacingDown
applyTransform Rotate180 FacingLeft = FacingRight
applyTransform Rotate180 FacingDown = FacingUp
applyTransform Rotate180 FacingRight = FacingLeft

neighbourOrientationFront :: Heading -> CubeOrientation
neighbourOrientationFront FacingUp = Top
neighbourOrientationFront FacingLeft = LeftFace
neighbourOrientationFront FacingRight = RightFace
neighbourOrientationFront FacingDown = Bottom

neighbourOrientationTop :: Heading -> CubeOrientation
neighbourOrientationTop FacingUp = RightFace
neighbourOrientationTop FacingLeft = Back
neighbourOrientationTop FacingRight = Front
neighbourOrientationTop FacingDown = LeftFace

neighbourOrientationLeft :: Heading -> CubeOrientation
neighbourOrientationLeft FacingUp = Bottom
neighbourOrientationLeft FacingLeft = Front
neighbourOrientationLeft FacingRight = Back
neighbourOrientationLeft FacingDown = Top

neighbourOrientationRight :: Heading -> CubeOrientation
neighbourOrientationRight FacingUp = Front
neighbourOrientationRight FacingLeft = Bottom
neighbourOrientationRight FacingRight = Top
neighbourOrientationRight FacingDown = Back

neighbourOrientationBottom :: Heading -> CubeOrientation
neighbourOrientationBottom FacingUp = Back
neighbourOrientationBottom FacingLeft = RightFace
neighbourOrientationBottom FacingRight = LeftFace
neighbourOrientationBottom FacingDown = Front

neighbourOrientationBack :: Heading -> CubeOrientation
neighbourOrientationBack FacingUp = LeftFace
neighbourOrientationBack FacingLeft = Top
neighbourOrientationBack FacingRight = Bottom
neighbourOrientationBack FacingDown = RightFace

combineTransforms :: Transform -> Transform -> Transform
combineTransforms None x = x
combineTransforms x None = x
combineTransforms RotateLeft RotateRight = None
combineTransforms RotateLeft Rotate180 = RotateRight
combineTransforms RotateLeft RotateLeft = Rotate180
combineTransforms RotateRight RotateRight = Rotate180
combineTransforms RotateRight Rotate180 = RotateLeft
combineTransforms RotateRight RotateLeft = None
combineTransforms Rotate180 RotateRight = RotateLeft
combineTransforms Rotate180 Rotate180 = None
combineTransforms Rotate180 RotateLeft = RotateRight

currentTile :: State -> BoardItem
currentTile (StateFlat board c _) = board M.! c
currentTile (StateCube (Cube _ faces) currentFace c _) =
    let CubeFace board _ _ = faces M.! currentFace
    in  case M.lookup c board of
            Nothing -> error ("Can't lookup coord: " ++ show c ++ " " ++ show board)
            Just x -> x