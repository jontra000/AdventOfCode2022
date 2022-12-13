module P7 (run1, run2, inputLocation) where

data Directory = Directory { subDirs :: [Directory], dirSize :: Int } deriving Show

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input7"

parse :: String -> Directory
parse = returnToRoot . foldl parseLine [Directory [] 0] . lines

returnToRoot :: [Directory] -> Directory
returnToRoot [root] = root
returnToRoot dirStack = returnToRoot $ stepOut dirStack

parseLine :: [Directory] -> String -> [Directory]
parseLine dirStack "$ cd /" = dirStack
parseLine dirStack "$ cd .." = stepOut dirStack
parseLine dirStack "$ ls" = dirStack
parseLine dirStack line = parseLine' (words line) dirStack

parseLine' :: [String] -> [Directory] -> [Directory]
parseLine' ("dir":_) dirStack = dirStack
parseLine' ("$":"cd":_) dirStack = Directory [] 0 : dirStack
parseLine' (fileSizeStr:_) (currentDir:dirStack) = updateDirSize currentDir (read fileSizeStr) : dirStack
parseLine' line _ = error ("error parsing input line: " ++ unwords line)

updateDirSize :: Directory -> Int -> Directory
updateDirSize (Directory currentSubDirs currentDirSize) fileSize = Directory currentSubDirs (currentDirSize + fileSize)

stepOut :: [Directory] -> [Directory]
stepOut (subDir:parentDir:dirStack) = addSubDir subDir parentDir : dirStack
stepOut _ = error "Tried to step out of top directory."

addSubDir :: Directory -> Directory -> Directory
addSubDir subDir parentDir = Directory (subDir : subDirs parentDir) (dirSize subDir + dirSize parentDir)

solve1 :: Directory -> Int
solve1 = sum . filter (<= 100000) . allDirSizes

solve2 :: Directory -> Int
solve2 baseDir = smallestDirOverSize (spaceToFree baseDir) baseDir

smallestDirOverSize :: Int -> Directory -> Int
smallestDirOverSize size = minimum . filter (> size) . allDirSizes

spaceToFree :: Directory -> Int
spaceToFree = (\x -> x-40000000) . dirSize

allDirSizes :: Directory -> [Int]
allDirSizes = map dirSize . allDirs

allDirs :: Directory -> [Directory]
allDirs directory = directory : concatMap allDirs (subDirs directory)