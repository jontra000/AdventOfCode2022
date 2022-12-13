module P11 (run1, run2, inputLocation) where

import Data.List.Split (chunksOf)
import qualified Data.Map as M
import Data.List (sort)

data Monkey = Monkey { monkeyItems :: [Integer], monkeyOperation :: Integer -> Integer, monkeyTestFactor :: Integer, nextMonkey :: Bool -> Int, monkeyInspections :: Int }
type State = M.Map Int Monkey

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input11"

parse :: String -> State
parse = M.fromList . zip [0..] . map parseMonkey . chunksOf 7 . lines

parseMonkey :: [String] -> Monkey
parseMonkey (_:itemsStr:operationStr:testStr:nextTrueStr:nextFalseStr:_) =
     Monkey (parseItems itemsStr) (parseOperation operationStr) (parseTest testStr) (parseNext nextFalseStr nextTrueStr) 0
parseMonkey e = error ("Can't parse monkey chunk: " ++ unlines e)

parseNext :: String -> String -> Bool -> Int
parseNext _ trueStr True = parseNext' trueStr
parseNext falseStr _ False = parseNext' falseStr

parseNext' :: String -> Int
parseNext' = read . last . words

parseTest :: String -> Integer
parseTest = read . last . words

parseOperation :: String -> (Integer -> Integer)
parseOperation = parseOperation' . reverse . words
    where parseOperation' ("old":"*":"old":_) = \x -> x*x
          parseOperation' (val:"*":_) = \x -> x * read val
          parseOperation' (val:"+":_) = \x -> x + read val
          parseOperation' e = error ("Couldn't parse test operation: " ++ unwords e)

parseItems :: String -> [Integer]
parseItems = reverse . map parseItem . drop 2 . words

parseItem :: String -> Integer
parseItem s = if last s == ',' then read (init s) else read s

solve1 :: State -> Int
solve1 = monkeyBusiness . doRounds manageWorry1 20

solve2 :: State -> Int
solve2 state = monkeyBusiness $ doRounds (manageWorry2 state) 10000 state

doRounds :: (Integer -> Integer) -> Int -> State -> State
doRounds manageWorry i = (!! i) . iterate (doRound manageWorry)

doRound :: (Integer -> Integer) -> State -> State
doRound manageWorry state = foldl (monkeyTurn manageWorry) state [0..7]

monkeyTurn :: (Integer -> Integer) -> State -> Int -> State
monkeyTurn manageWorry state i =
    let monkey = state M.! i
        state' = inspectItems manageWorry state monkey
    in  endMonkeyTurn state' i

inspectItems :: (Integer -> Integer) -> State -> Monkey -> State
inspectItems manageWorry state monkey = foldl (inspectItem manageWorry monkey) state (reverse (monkeyItems monkey))

inspectItem :: (Integer -> Integer) -> Monkey -> State -> Integer -> State
inspectItem manageWorry monkey state worryLevel =
    let worryLevel' = manageWorry $ monkeyOperation monkey worryLevel
        receivingMonkey = nextMonkey monkey (testItem monkey worryLevel')
    in  updateReceivingMonkey state receivingMonkey worryLevel'

testItem :: Monkey -> Integer -> Bool
testItem monkey worryLevel = worryLevel `mod` monkeyTestFactor monkey == 0

updateReceivingMonkey :: State -> Int -> Integer -> State
updateReceivingMonkey state monkeyIndex item = M.adjust (addItem item) monkeyIndex state

addItem :: Integer -> Monkey ->  Monkey
addItem item (Monkey items op test next inspections) = Monkey (item : items) op test next inspections

endMonkeyTurn :: State -> Int -> State
endMonkeyTurn state i = M.adjust monkey' i state
    where monkey' monkey = Monkey [] (monkeyOperation monkey) (monkeyTestFactor monkey) (nextMonkey monkey) (inspections monkey)
          inspections monkey = monkeyInspections monkey + length (monkeyItems monkey)

monkeyBusiness :: State -> Int
monkeyBusiness = product . take 2 . reverse . sort . map monkeyInspections . M.elems

manageWorry1 :: Integer -> Integer
manageWorry1 x = x `div` 3

manageWorry2 :: State -> Integer -> Integer
manageWorry2 state x = x `mod` commonWorryFactor state

commonWorryFactor :: State -> Integer
commonWorryFactor = product . map monkeyTestFactor . M.elems