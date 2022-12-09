module P2b (run) where

data Shape = Rock | Paper | Scissors deriving Eq
data Result = Lose | Draw | Win
data Round = Round Shape Result

run :: String -> Int
run = solve . parse

parse :: String -> [Round]
parse = map parseLine . lines

parseLine :: String -> Round
parseLine (opponentShapeChar:_:myShapeChar:_) = Round (parseOpponentShape opponentShapeChar) (parseResult myShapeChar)
parseLine e = error ("Malformed line: " ++ e)

parseOpponentShape :: Char -> Shape
parseOpponentShape 'A' = Rock
parseOpponentShape 'B' = Paper
parseOpponentShape 'C' = Scissors
parseOpponentShape c = error ("Unknown opponent shape code: " ++ [c])

parseResult :: Char -> Result
parseResult 'X' = Lose
parseResult 'Y' = Draw
parseResult 'Z' = Win
parseResult c = error ("Unknown result code: " ++ [c])

solve :: [Round] -> Int
solve = sum . map score

score :: Round -> Int
score (Round opponentShape result) = outcomeScore result + shapeScore (myShape opponentShape result)

outcomeScore :: Result -> Int
outcomeScore Lose = 0
outcomeScore Draw = 3
outcomeScore Win = 6

myShape :: Shape -> Result -> Shape
myShape shape Draw = shape
myShape Rock Win = Paper
myShape Paper Win = Scissors
myShape Scissors Win = Rock
myShape Rock Lose = Scissors
myShape Paper Lose = Rock
myShape Scissors Lose = Paper

shapeScore :: Shape -> Int
shapeScore Rock = 1
shapeScore Paper = 2
shapeScore Scissors = 3