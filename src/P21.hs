module P21 (run1, run2, inputLocation) where

import qualified Data.Map as M

data Operation = Plus | Minus | Multiply | Divide
data JobResult = NumberResult Int | VariableResult (Int -> Int)
data MonkeyJob = ShoutNumber Int | ShoutResult String String Operation
type MonkeyJobs = M.Map String MonkeyJob

inputLocation :: String
inputLocation = "inputs/input21"

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

solve2 :: MonkeyJobs -> Int
solve2 jobs = variableResult jobs (jobs M.! "root")

parse :: String -> MonkeyJobs
parse = M.fromList . map parseLine . lines

parseLine :: String -> (String, MonkeyJob)
parseLine = parseLine' . words

parseLine' :: [String] -> (String, MonkeyJob)
parseLine' [nameStr, numberStr] = (init nameStr, ShoutNumber (read numberStr))
parseLine' [nameStr, dependencyStr1, operationStr, dependencyStr2] = (init nameStr, shoutResult dependencyStr1 dependencyStr2 operationStr)
parseLine' e = error ("Error parsing input line: " ++ unwords e)

shoutResult :: String -> String -> String -> MonkeyJob
shoutResult dependency1 dependency2 operationStr = ShoutResult dependency1 dependency2 (parseOperation operationStr)

parseOperation :: String -> Operation
parseOperation "+" = Plus
parseOperation "-" = Minus
parseOperation "*" = Multiply
parseOperation "/" = Divide
parseOperation e = error ("Couldn't parse operation " ++ e)

solve1 :: MonkeyJobs -> Int
solve1 jobs = result jobs (jobs M.! "root")

result :: MonkeyJobs -> MonkeyJob -> Int
result _ (ShoutNumber x) = x
result jobs (ShoutResult dep1 dep2 operation) =
    let resultLeft = result jobs (jobs M.! dep1)
        resultRight = result jobs (jobs M.! dep2)
    in  doOperation operation resultLeft resultRight

doOperation :: Operation -> Int -> Int -> Int
doOperation Plus x y = x + y
doOperation Minus x y = x - y
doOperation Multiply x y = x * y
doOperation Divide x y = x `div` y

result2 :: MonkeyJobs -> MonkeyJob -> JobResult
result2 _ (ShoutNumber x) = NumberResult x
result2 jobs (ShoutResult "humn" dep2 operation) = reverseOperationRight id (result jobs (jobs M.! dep2)) operation
result2 jobs (ShoutResult dep1 "humn" operation) = reverseOperationLeft id (result jobs (jobs M.! dep1)) operation
result2 jobs (ShoutResult dep1 dep2 operation) = case (result2 jobs (jobs M.! dep1), result2 jobs (jobs M.! dep2)) of
        (NumberResult x, NumberResult y) -> NumberResult $ doOperation operation x y
        (VariableResult v, NumberResult y) -> reverseOperationRight v y operation
        (NumberResult x, VariableResult v) -> reverseOperationLeft v x operation
        _ -> error "Diamond dependency"

reverseOperationLeft :: (Int -> Int) -> Int -> Operation -> JobResult
reverseOperationLeft f y Plus = VariableResult (f . (\acc -> acc - y))
reverseOperationLeft f y Minus = VariableResult (f . (y-)) -- x - y = acc : acc + y
reverseOperationLeft f y Multiply = VariableResult (f . (`div` y))
reverseOperationLeft f y Divide = VariableResult (f . (y `div`))

reverseOperationRight :: (Int -> Int) -> Int -> Operation -> JobResult
reverseOperationRight f x Plus = VariableResult (f . (\acc -> acc - x))
reverseOperationRight f x Minus = VariableResult (f . (x +))
reverseOperationRight f x Multiply = VariableResult (f . (`div` x))
reverseOperationRight f x Divide = VariableResult (f . (*) x)

variableResult :: MonkeyJobs -> MonkeyJob -> Int
variableResult jobs (ShoutResult dep1 dep2 _) = case (result2 jobs (jobs M.! dep1), result2 jobs (jobs M.! dep2)) of
        (VariableResult v, NumberResult y) -> v y
        (NumberResult x, VariableResult v) -> v x
        _ -> error "Didn't get variable result"
variableResult _ _ = error "Top level should be operation"