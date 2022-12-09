module P7a (run) where

data Directory = Directory { subDirs :: [Directory], dirSize :: Int } deriving Show

run :: String -> Int
run = solve . parse

parse :: String -> Directory
parse = fst . parse' (Directory [] 0) . lines

parse' :: Directory -> [String] -> (Directory, [String])
parse' directory [] = (directory, [])
parse' directory ("$ cd /":xs) = parse' directory xs
parse' directory ("$ cd ..":xs) = (directory, xs)
parse' directory ("$ ls":xs) = parse' directory xs
parse' directory (x:xs) = parse'' directory (words x) xs

parse'' :: Directory -> [String] -> [String] -> (Directory, [String])
parse'' (Directory contents size) ("$":"cd":_:_) xs =
    let (subDir, remainingLines) = parse' (Directory [] 0) xs
    in  parse' (Directory (subDir : contents) (size + dirSize subDir)) remainingLines
parse'' directory ("dir":_) xs = parse' directory xs
parse'' (Directory contents size) (fileSize:_) xs = parse' (Directory contents (size + read fileSize)) xs
parse'' _ x _ = error ("Couldn't parse line " ++ unwords x)

solve :: Directory -> Int
solve = sum . filter (<= 100000) . map dirSize . allDirs

allDirs :: Directory -> [Directory]
allDirs directory = directory : concatMap allDirs (subDirs directory)