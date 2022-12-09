module P4a (run) where

import Data.List (intersect)
import Data.List.Split (splitOn)

type ElfPair = ([Int], [Int])

run :: String -> Int
run = solve . parse

parse :: String -> [ElfPair]
parse = map parseLine . lines

parseLine :: String -> ElfPair
parseLine = parseRanges . splitElfPair

splitElfPair :: String -> (String, String)
splitElfPair s =
    let (elf1:elf2:_) = splitOn "," s
    in  (elf1, elf2)

parseRanges :: (String, String) -> ElfPair
parseRanges (elf1, elf2) = (parseRange elf1, parseRange elf2)

parseRange :: String -> [Int]
parseRange s =
    let (rangeStart:rangeEnd:_) = splitOn "-" s
    in  [(read rangeStart)..(read rangeEnd)]

solve :: [ElfPair] -> Int
solve = length . filter fullyContains

fullyContains :: ElfPair -> Bool
fullyContains (range1, range2) =
    let intersection = length $ intersect range1 range2
    in  intersection == length range1 || intersection == length range2