module P22 (run1, run2, inputLocation) where

import qualified Data.Map as M
import Data.List.Split (splitOn)
import Data.Char (isNumber)
import Data.Maybe (catMaybes)
import Data.Tuple (swap)
import Data.List (delete, (\\))

data Direction = Forward | TurnLeft | TurnRight
type Coord = (Int, Int)
data Tile = Wall | Open deriving Show
type Board = M.Map Coord Tile
type CubeFaces = M.Map FaceName CubeFace
data Cube = Cube Int CubeFaces deriving Show
data Input = Input Board [Direction]
data Heading = HeadingUp | HeadingDown | HeadingLeft | HeadingRight deriving (Eq, Show)
data State = StateFlat Board Coord Heading | StateCube Cube FaceName Coord Heading deriving Show
data FaceName = Front | LeftFace | RightFace | Top | Bottom | Back deriving (Eq, Ord, Show)
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
parse = parseParagraphs . splitOn [""] . lines

parseParagraphs :: [[String]] -> Input
parseParagraphs (boardStr:[directionsStr]:_) = Input (parseBoard boardStr) (parseDirections directionsStr)
parseParagraphs e = error ("Error parsing input " ++ unlines (concat e))

parseDirections :: String -> [Direction]
parseDirections [] = []
parseDirections ('L':res) = TurnLeft : parseDirections res
parseDirections ('R':res) = TurnRight : parseDirections res
parseDirections s =
    let (numberStr,res) = span isNumber s
    in  replicate (read numberStr) Forward ++ parseDirections res

parseBoard :: [String] -> Board
parseBoard = M.fromList . concat . zipWith parseBoardLine [1..]

parseBoardLine :: Int -> String -> [(Coord, Tile)]
parseBoardLine y = catMaybes . zipWith (parseTile y) [1..]

parseTile :: Int -> Int -> Char -> Maybe (Coord, Tile)
parseTile y x '.' = Just ((x,y), Open)
parseTile y x '#' = Just ((x,y), Wall)
parseTile _ _ _ = Nothing

solve1 :: Input -> Int
solve1 (Input board directions) = solve (initStateFlat board) directions 

solve2 :: Input -> Int
solve2 (Input board directions) = solve (initStateCube board) directions 

solve :: State -> [Direction] -> Int
solve state = finalPassword . foldl applyDirection state

initStateFlat :: Board -> State
initStateFlat board = StateFlat board (topLeftLocation board) HeadingRight

initStateCube :: Board -> State
initStateCube board = StateCube (makeCube board) Front (1,1) HeadingRight

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
            Open -> state'

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
wrapFlat' HeadingDown = wrapDown
wrapFlat' HeadingUp = wrapUp
wrapFlat' HeadingLeft = wrapLeft
wrapFlat' HeadingRight = wrapRight

wrapCube :: Cube -> FaceName -> Coord -> Heading -> State
wrapCube cube@(Cube sideLength _) face location heading =
    let face' = moveFace face heading
        heading' = wrapHeadingCube heading
        location' = wrapLocationCube sideLength heading location
    in  StateCube cube face' location' heading'

moveFace :: FaceName -> Heading -> FaceName
moveFace Front HeadingUp = Top
moveFace Front HeadingLeft = LeftFace
moveFace Front HeadingRight = RightFace
moveFace Front HeadingDown = Bottom
moveFace Bottom HeadingUp = Back
moveFace Bottom HeadingLeft = RightFace
moveFace Bottom HeadingRight = LeftFace
moveFace Bottom HeadingDown = Front
moveFace Top HeadingUp = RightFace
moveFace Top HeadingLeft = Back
moveFace Top HeadingRight = Front
moveFace Top HeadingDown = LeftFace
moveFace LeftFace HeadingUp = Bottom
moveFace LeftFace HeadingLeft = Front
moveFace LeftFace HeadingRight = Back
moveFace LeftFace HeadingDown = Top
moveFace RightFace HeadingUp = Front
moveFace RightFace HeadingLeft = Bottom
moveFace RightFace HeadingRight = Top
moveFace RightFace HeadingDown = Back
moveFace Back HeadingUp = LeftFace
moveFace Back HeadingLeft = Top
moveFace Back HeadingRight = Bottom
moveFace Back HeadingDown = RightFace

wrapHeadingCube :: Heading -> Heading
wrapHeadingCube HeadingUp = HeadingLeft
wrapHeadingCube HeadingLeft = HeadingRight
wrapHeadingCube HeadingRight = HeadingDown
wrapHeadingCube HeadingDown = HeadingUp

wrapLocationCube :: Int -> Heading -> Coord -> Coord
wrapLocationCube sideLength HeadingUp (x,_) = (sideLength, sideLength + 1 - x)
wrapLocationCube sideLength HeadingLeft (_,y) = (1, sideLength + 1 - y)
wrapLocationCube sideLength HeadingRight (_,y) = (sideLength + 1 - y, 1)
wrapLocationCube sideLength HeadingDown (x,_) = (sideLength + 1 - x, sideLength)

wrapDown :: Board -> Coord -> Coord
wrapDown board (column,_) = minimum $ filter (\(x,_) -> x == column) $ M.keys board

wrapUp :: Board -> Coord -> Coord
wrapUp board (column,_) = maximum $ filter (\(x,_) -> x == column) $ M.keys board

wrapLeft :: Board -> Coord -> Coord
wrapLeft board (_,row) = maximum $ filter (\(_,y) -> y==row) $ M.keys board

wrapRight :: Board -> Coord -> Coord
wrapRight board (_,row) = minimum $ filter (\(_,y) -> y==row) $ M.keys board

moveForward' :: Coord -> Heading -> Coord
moveForward' (x,y) HeadingDown = (x, y+1)
moveForward' (x,y) HeadingUp = (x, y-1)
moveForward' (x,y) HeadingLeft = (x-1, y)
moveForward' (x,y) HeadingRight = (x+1, y)

turnRight :: State -> State
turnRight (StateFlat board location facing) = StateFlat board location (turnRight' facing)
turnRight (StateCube cube orientation location facing) = StateCube cube orientation location (turnRight' facing)

turnRight' :: Heading -> Heading
turnRight' HeadingDown = HeadingLeft
turnRight' HeadingLeft = HeadingUp
turnRight' HeadingUp = HeadingRight
turnRight' HeadingRight = HeadingDown

turnLeft :: State -> State
turnLeft (StateFlat board location facing) = StateFlat board location (turnLeft' facing)
turnLeft (StateCube cube orientation location facing) = StateCube cube orientation location (turnLeft' facing)

turnLeft' :: Heading -> Heading
turnLeft' HeadingDown = HeadingRight
turnLeft' HeadingLeft = HeadingDown
turnLeft' HeadingUp = HeadingLeft
turnLeft' HeadingRight = HeadingUp

finalPassword :: State -> Int
finalPassword (StateFlat _ c facing) = finalPassword' c facing
finalPassword (StateCube cube orientation c facing) =
    let (flatCoords, flatFacing) = cubeLocationToFlatLocation cube orientation c facing
    in  finalPassword' flatCoords flatFacing

finalPassword' :: Coord -> Heading -> Int
finalPassword' (x,y) facing = 1000*y + 4*x + facingScore facing

cubeLocationToFlatLocation :: Cube -> FaceName -> Coord -> Heading -> (Coord, Heading)
cubeLocationToFlatLocation (Cube sideLength cube) orientation c facing =
    let cubeFace@(CubeFace _ _ transform) = cube M.! orientation
        c' = translateCubeCoords sideLength cubeFace c
        facing' = translateCubeFacing transform facing
    in  (c', facing')

translateCubeCoords :: Int -> CubeFace -> Coord -> Coord
translateCubeCoords sideLength (CubeFace _ (boardX, boardY) translation) c =
    let (x', y') = translateCubeCoords' sideLength (reverseTransform translation) c
    in  (boardX + x' - 1, boardY + y' - 1)

reverseTransform :: Transform -> Transform
reverseTransform RotateLeft = RotateRight
reverseTransform RotateRight = RotateLeft
reverseTransform x = x

translateCubeCoords' :: Int -> Transform -> Coord -> Coord
translateCubeCoords' _ None c = c
translateCubeCoords' sideLength RotateLeft (x, y) = (sideLength + 1 - y, x)
translateCubeCoords' sideLength RotateRight (x, y) = (y, sideLength + 1- x)
translateCubeCoords' sideLength Rotate180 (x, y) = (sideLength + 1 - x, sideLength + 1 - y)

translateCubeFacing :: Transform -> Heading -> Heading
translateCubeFacing None h = h
translateCubeFacing RotateLeft HeadingUp = HeadingLeft
translateCubeFacing RotateLeft HeadingRight = HeadingUp
translateCubeFacing RotateLeft HeadingDown = HeadingRight
translateCubeFacing RotateLeft HeadingLeft = HeadingDown
translateCubeFacing RotateRight HeadingUp = HeadingRight
translateCubeFacing RotateRight HeadingLeft = HeadingUp
translateCubeFacing RotateRight HeadingDown = HeadingLeft
translateCubeFacing RotateRight HeadingRight = HeadingDown
translateCubeFacing Rotate180 HeadingUp = HeadingDown
translateCubeFacing Rotate180 HeadingLeft = HeadingRight
translateCubeFacing Rotate180 HeadingDown = HeadingUp
translateCubeFacing Rotate180 HeadingRight = HeadingLeft

facingScore :: Heading -> Int
facingScore HeadingRight = 0
facingScore HeadingDown = 1
facingScore HeadingLeft = 2
facingScore HeadingUp = 3

makeCube :: Board -> Cube
makeCube board = Cube faceLength' faces
    where faceLength' = faceLength board
          faces = constructFaces faceLength' board

faceLength :: Board -> Int
faceLength board =
    let coords = M.keys board
        maxX = maximum $ map fst coords
        maxY = maximum $ map snd coords
        result = max maxX maxY `div` 4
    in  validateFaceLength maxX maxY result

validateFaceLength :: Int -> Int -> Int -> Int
validateFaceLength maxX maxY result
    | maxX `mod` result == 0 && maxY `mod` result == 0 && (maxY `div` 3 == result || maxX `div` 3 == result) = result
    | otherwise = error "Input not 3x4 grid"

faceCoords :: Int -> Board -> [Coord]
faceCoords gridSize board = filter (`M.member` board) [(x,y) | x <- gridPoints, y <- gridPoints]
    where gridPoints = map (+1) [0, gridSize, gridSize*2, gridSize*3]

constructFaces :: Int -> Board -> CubeFaces
constructFaces sideLength board =
    let faceCoords' = faceCoords sideLength board
        firstFace = createFrontFace sideLength faceCoords' board
        initFaces = M.singleton Front firstFace
    in  addCubeNeighbours board sideLength faceCoords' Front firstFace initFaces

startLocation :: [Coord] -> Coord
startLocation = swap . minimum . map swap -- Will sort by lowest x value by default, do a swap to sort by lowest y value

addCubeNeighbours :: Board -> Int -> [Coord] -> FaceName -> CubeFace -> CubeFaces -> CubeFaces
addCubeNeighbours board gridSize remainingLocations currentFace face@(CubeFace _ currentLocation _) currentCube =
    let neighbours = neighbouringFaces gridSize currentLocation remainingLocations 
        remainingLocations' = delete currentLocation (remainingLocations \\ neighbours)
    in  foldl (addFace board gridSize remainingLocations' currentFace face) currentCube neighbours

neighbouringFaces :: Int -> Coord -> [Coord] -> [Coord]
neighbouringFaces gridSize currentLocation = filter (isNeighbour gridSize currentLocation)

isNeighbour :: Int -> Coord -> Coord -> Bool
isNeighbour sideLength (x1, y1) (x2, y2)
    | (x1 == x2) && dy == sideLength = True
    | (y1 == y2) && dx == sideLength = True
    | otherwise = False
        where dx = abs (x1 - x2)
              dy = abs (y1 - y2)

addFace :: Board -> Int -> [Coord] -> FaceName -> CubeFace -> CubeFaces -> Coord -> CubeFaces
addFace board gridSize remainingLocations connectingOrientation (CubeFace _ connectingLocation connectingTransform) cube currentLocation =
    let heading = neighbourHeading connectingTransform connectingLocation currentLocation
        orientation = neighbourOrientation connectingOrientation heading
        transform = neighbourTransform connectingTransform heading
        face = createFace gridSize transform currentLocation board
        cube' = M.insert orientation face cube
    in  addCubeNeighbours board gridSize remainingLocations orientation face cube'

createFace :: Int -> Transform -> Coord -> Board -> CubeFace
createFace gridSize transform currentLocation board =
    let faceBoard' = faceBoard gridSize transform currentLocation board
    in  CubeFace faceBoard' currentLocation transform

createFrontFace :: Int -> [Coord] -> Board -> CubeFace
createFrontFace gridSize faceLocations board =
    let startLocation' = startLocation faceLocations
    in  createFace gridSize None startLocation' board

faceBoard :: Int -> Transform -> Coord -> Board -> Board
faceBoard sideLength transform c = transformBoard sideLength transform . subBoard sideLength c

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

neighbourOrientation :: FaceName -> Heading -> FaceName
neighbourOrientation Front = neighbourOrientationFront
neighbourOrientation Top = neighbourOrientationTop
neighbourOrientation LeftFace = neighbourOrientationLeft
neighbourOrientation RightFace = neighbourOrientationRight
neighbourOrientation Bottom = neighbourOrientationBottom
neighbourOrientation Back = neighbourOrientationBack

neighbourHeading :: Transform -> Coord -> Coord -> Heading
neighbourHeading transform cPrev = translateCubeFacing (reverseTransform transform) . neighbourHeading' cPrev

neighbourHeading' :: Coord -> Coord -> Heading
neighbourHeading' cPrev@(connectingX, connectingY) cNext@(x, y)
    | connectingX == x && connectingY > y = HeadingUp
    | connectingX == x && connectingY < y = HeadingDown
    | connectingY == y && connectingX > x = HeadingLeft
    | connectingY == y && connectingX < x = HeadingRight
    | otherwise = error ("non-aligned face coordinates: " ++ show cPrev ++ " and " ++ show cNext)

neighbourTransform :: Transform -> Heading -> Transform
neighbourTransform transform = combineTransforms transform . neighbourTransform'

neighbourTransform' :: Heading -> Transform
neighbourTransform' HeadingUp = RotateRight
neighbourTransform' HeadingRight = RotateLeft
neighbourTransform' HeadingDown = Rotate180
neighbourTransform' HeadingLeft = Rotate180

neighbourOrientationFront :: Heading -> FaceName
neighbourOrientationFront HeadingUp = Top
neighbourOrientationFront HeadingLeft = LeftFace
neighbourOrientationFront HeadingRight = RightFace
neighbourOrientationFront HeadingDown = Bottom

neighbourOrientationTop :: Heading -> FaceName
neighbourOrientationTop HeadingUp = RightFace
neighbourOrientationTop HeadingLeft = Back
neighbourOrientationTop HeadingRight = Front
neighbourOrientationTop HeadingDown = LeftFace

neighbourOrientationLeft :: Heading -> FaceName
neighbourOrientationLeft HeadingUp = Bottom
neighbourOrientationLeft HeadingLeft = Front
neighbourOrientationLeft HeadingRight = Back
neighbourOrientationLeft HeadingDown = Top

neighbourOrientationRight :: Heading -> FaceName
neighbourOrientationRight HeadingUp = Front
neighbourOrientationRight HeadingLeft = Bottom
neighbourOrientationRight HeadingRight = Top
neighbourOrientationRight HeadingDown = Back

neighbourOrientationBottom :: Heading -> FaceName
neighbourOrientationBottom HeadingUp = Back
neighbourOrientationBottom HeadingLeft = RightFace
neighbourOrientationBottom HeadingRight = LeftFace
neighbourOrientationBottom HeadingDown = Front

neighbourOrientationBack :: Heading -> FaceName
neighbourOrientationBack HeadingUp = LeftFace
neighbourOrientationBack HeadingLeft = Top
neighbourOrientationBack HeadingRight = Bottom
neighbourOrientationBack HeadingDown = RightFace

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

currentTile :: State -> Tile
currentTile (StateFlat board c _) = board M.! c
currentTile (StateCube (Cube _ faces) currentFace c _) =
    let CubeFace board _ _ = faces M.! currentFace
    in  case M.lookup c board of
            Nothing -> error ("Can't lookup coord: " ++ show c ++ " " ++ show board)
            Just x -> x