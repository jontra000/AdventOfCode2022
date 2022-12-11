module P11 (run1, run2) where

import Data.List.Split (chunksOf)
import qualified Data.Map as M
import Data.List (sort)

data Monkey = Monkey { monkeyItems :: [Integer], monkeyOperation :: Integer -> Integer, monkeyTest :: Integer, monkeyNextFalse :: Int, monkeyNextTrue :: Int, monkeyInspections :: Int }
type State = M.Map Int Monkey

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

parse :: String -> State
parse = M.fromList . zip [0..] . map parseMonkey . chunksOf 7 . lines

parseMonkey :: [String] -> Monkey
parseMonkey (_:itemsStr:operationStr:testStr:nextTrueStr:nextFalseStr:_) =
     Monkey (parseItems itemsStr) (parseOperation operationStr) (parseTest testStr) (parseNext nextFalseStr) (parseNext nextTrueStr) 0
parseMonkey e = error ("Can't parse monkey chunk: " ++ unlines e)

parseNext :: String -> Int
parseNext = read . last . words

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
solve2 state = monkeyBusiness $ doRounds (manageWorry2 commonWorryFactor) 10000 state
    where commonWorryFactor = product $ map monkeyTest $ M.elems state

doRounds :: (Integer -> Integer) -> Int -> State -> State
doRounds manageWorry i = (!! i) . iterate (doRound manageWorry)

doRound :: (Integer -> Integer) -> State -> State
doRound manageWorry state = foldl (monkeyTurn manageWorry) state [0..7]

monkeyTurn :: (Integer -> Integer) -> State -> Int -> State
monkeyTurn manageWorry state i =
    let monkey = state M.! i
        state' = foldl (processItem manageWorry monkey) state (reverse (monkeyItems monkey))
    in  endMonkeyTurn state' i

processItem :: (Integer -> Integer) -> Monkey -> State -> Integer -> State
processItem manageWorry monkey state worryLevel =
    let worryLevel' = manageWorry $ monkeyOperation monkey worryLevel
        receivingMonkey = if testItem monkey worryLevel' then monkeyNextTrue monkey else monkeyNextFalse monkey
    in  updateReceivingMonkey state receivingMonkey worryLevel'

testItem :: Monkey -> Integer -> Bool 
testItem monkey worryLevel = worryLevel `mod` monkeyTest monkey == 0

updateReceivingMonkey :: State -> Int -> Integer -> State
updateReceivingMonkey state monkeyIndex item =
    M.adjust (addItem item) monkeyIndex state

addItem :: Integer -> Monkey ->  Monkey
addItem item (Monkey items op test nextFalse nextTrue inspections) = Monkey (item : items) op test nextFalse nextTrue inspections

endMonkeyTurn :: State -> Int -> State
endMonkeyTurn state i = M.adjust endMonkeyTurn' i state
    where endMonkeyTurn' monkey = Monkey [] (monkeyOperation monkey) (monkeyTest monkey) (monkeyNextFalse monkey) (monkeyNextTrue monkey) (inspections monkey)
          inspections monkey = monkeyInspections monkey + length (monkeyItems monkey)

monkeyBusiness :: State -> Int
monkeyBusiness = product . take 2 . reverse . sort . map monkeyInspections . M.elems

manageWorry1 :: Integer -> Integer
manageWorry1 x = x `div` 3

manageWorry2 :: Integer -> Integer -> Integer
manageWorry2 commonWorryFactor x = x `mod` commonWorryFactor