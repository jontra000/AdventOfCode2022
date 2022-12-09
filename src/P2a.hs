module P2a (run) where

data Shape = Rock | Paper | Scissors deriving Eq
data Round = Round Shape Shape

run :: String -> Int
run = solve . parse

parse :: String -> [Round]
parse = map parseLine . lines

parseLine :: String -> Round
parseLine (opponentShapeChar:_:myShapeChar:_) = Round (parseOpponentShape opponentShapeChar) (parseMyShape myShapeChar)
parseLine e = error ("Malformed line: " ++ e)

parseOpponentShape :: Char -> Shape
parseOpponentShape 'A' = Rock
parseOpponentShape 'B' = Paper
parseOpponentShape 'C' = Scissors
parseOpponentShape c = error ("Unknown opponent shape code: " ++ [c])

parseMyShape :: Char -> Shape
parseMyShape 'X' = Rock
parseMyShape 'Y' = Paper
parseMyShape 'Z' = Scissors
parseMyShape c = error ("Unknown my shape code: " ++ [c])

solve :: [Round] -> Int
solve = sum . map score

score :: Round -> Int
score (Round opponentShape myShape) = outcomeScore opponentShape myShape + shapeScore myShape

outcomeScore :: Shape -> Shape -> Int
outcomeScore opponentShape myShape
  | opponentShape == myShape = 3
  | opponentShape `beats` myShape = 0
  | otherwise = 6

shapeScore :: Shape -> Int
shapeScore Rock = 1
shapeScore Paper = 2
shapeScore Scissors = 3

beats :: Shape -> Shape -> Bool
beats Rock Scissors = True
beats Paper Rock = True
beats Scissors Paper = True
beats _ _ = False