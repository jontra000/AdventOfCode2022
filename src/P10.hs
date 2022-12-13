module P10 (run1, run2, inputLocation) where
import Data.List.Split (chunksOf)

data Instruction = Noop | AddX Int

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> String
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input10"

parse :: String -> [Instruction]
parse = map parseLine . lines

parseLine :: String -> Instruction
parseLine "noop" = Noop
parseLine s = parseAddX $ words s

parseAddX :: [String] -> Instruction
parseAddX ("addx":valueStr:_) = AddX (read valueStr)
parseAddX e = error ("Can't parse input line: " ++ unwords e)

solve1 :: [Instruction] -> Int
solve1 = sum . targetCycles . signalStrengths

signalStrengths :: [Instruction] -> [Int]
signalStrengths = zipWith (*) [1..] . xValues

xValues :: [Instruction] -> [Int]
xValues = scanl (+) 1 . concatMap xValueDeltas

xValueDeltas :: Instruction -> [Int]
xValueDeltas Noop = [0]
xValueDeltas (AddX delta) = [0, delta]

targetCycles :: [Int] -> [Int]
targetCycles xs = map (xs !!) targetIndices

targetIndices :: [Int]
targetIndices = map (\x -> x-1) [20, 60, 100, 140, 180, 220]

solve2 :: [Instruction] -> String
solve2 = concatMap ((++ ['\n']) . zipWith renderScreen [0..]) . chunksOf 40 . xValues

renderScreen :: Int -> Int -> Char
renderScreen crtLoc spriteLoc = if abs (crtLoc - spriteLoc) > 1 then '.' else '#'
