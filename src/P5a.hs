module P5a (run) where

import Data.List.Split (splitOn)
import Data.Char (isDigit)
import qualified Data.Map as M

type State = M.Map Int [Char]
type Instructions = [Instruction]
data Instruction = Instruction { instructionSource :: Int, instructionDestination :: Int, instructionCount :: Int } deriving Show
data Input = Input State Instructions deriving Show

run :: String -> String
run = solve . parse

parse :: String -> Input
parse = parseInput . splitInput

splitInput :: String -> [[String]]
splitInput = splitOn [""] . lines

parseInput :: [[String]] -> Input
parseInput (stateStr:instructionsStr:_) = Input (parseState stateStr) (parseInstructions instructionsStr)
parseInput e = error ("Invalid input data: " ++ unlines (concat e))

parseState :: [String] -> State
parseState = allocateContainers . parseContainers . tail . reverse

parseContainers :: [String] -> [[Char]]
parseContainers = foldl parseState' (replicate 9 [])

allocateContainers :: [[Char]] -> State
allocateContainers = M.fromList . zip [1..]

parseState' :: [[Char]] -> String -> [[Char]]
parseState' state input =
    let contentIndices = take 9 [1,5..]
        containers = map (input !!) contentIndices
    in  zipWith addContainers state containers

addContainers :: [Char] -> Char -> [Char]
addContainers x ' ' = x
addContainers x c = c : x

parseInstructions :: [String] -> Instructions
parseInstructions = map parseInstruction

parseInstruction :: String -> Instruction
parseInstruction = instruction . parseNumbers

parseNumbers :: String -> [Int]
parseNumbers = map read . filter isNumber . words

isNumber :: String -> Bool
isNumber = all isDigit

instruction :: [Int] -> Instruction
instruction (count:source:destination:_) = Instruction source destination count
instruction e = error ("malformed instruction line: " ++ show e)

solve :: Input -> String
solve (Input state instructions) = topContainers $ foldl applyInstruction state instructions

applyInstruction :: State -> Instruction -> State
applyInstruction state (Instruction source destination count) = moveContainers source destination state !! count

moveContainers :: Int -> Int -> State -> [State]
moveContainers source destination = iterate (moveContainer source destination)

moveContainer :: Int -> Int -> State -> State
moveContainer source destination state = updateState source (state M.! source) destination state

updateState :: Int -> [Char] -> Int -> State -> State
updateState source (x:sourceContainers) destination state = M.insert destination destinationContainers $ M.insert source sourceContainers state
    where destinationContainers = x : (state M.! destination)
updateState source _ _ _ = error ("No containers left at location " ++ show source)

topContainers :: State -> String
topContainers state = map (topContainer state) [1..9]

topContainer :: State -> Int -> Char
topContainer state i = head $ state M.! i