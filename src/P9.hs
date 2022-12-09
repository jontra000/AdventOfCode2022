module P9 (run1, run2) where
import Data.List (nub)

data Direction = Up | Down | Left | Right
type Coord = (Int, Int)

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

parse :: String -> [Direction]
parse = concatMap parseLine . lines

parseLine :: String -> [Direction]
parseLine = parseWords . words

parseWords :: [String] -> [Direction]
parseWords (dirStr:countStr:_) = replicate (read countStr) (parseDirection dirStr)
parseWords e = error ("Error parsing input: " ++ unwords e)

parseDirection :: String -> Direction
parseDirection "U" = Up
parseDirection "D" = Down
parseDirection "L" = P9.Left
parseDirection "R" = P9.Right
parseDirection x = error ("Unknown direction token: " ++ x)

solve1 :: [Direction] -> Int
solve1 = length . nub . tailPositions . headPositions

solve2 :: [Direction] -> Int
solve2 = length . nub . (!! 9) . iterate tailPositions . headPositions

headPositions :: [Direction] -> [Coord]
headPositions = scanl applyMotion (0,0)

applyMotion :: Coord -> Direction -> Coord
applyMotion (x,y) Up = (x, y+1)
applyMotion (x,y) Down = (x, y-1)
applyMotion (x,y) P9.Left = (x-1, y)
applyMotion (x,y) P9.Right = (x+1, y)

tailPositions :: [Coord] -> [Coord]
tailPositions = scanl followHead (0,0)

followHead :: Coord -> Coord -> Coord
followHead (tailX, tailY) (headX, headY)
  | abs (tailX - headX) > 1 && abs (tailY - headY) > 1 = ((tailX + headX) `div` 2, (tailY + headY) `div` 2)
  | abs (tailX - headX) > 1 = ((tailX + headX) `div` 2, headY)
  | abs (tailY - headY) > 1 = (headX, (tailY + headY) `div` 2)
  | otherwise = (tailX, tailY)