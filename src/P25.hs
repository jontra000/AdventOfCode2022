module P25 (run1, run2, inputLocation) where

inputLocation :: String
inputLocation = "inputs/input25"

run1 :: String -> String
run1 = solve1 . parse

run2 :: String -> Int
run2 _ = 0 -- solve2 . parse

parse :: String -> [String]
parse = lines

solve1 :: [String] -> String
solve1 = convertToSnafu . sum . map convertFromSnafu

convertFromSnafu :: String -> Int
convertFromSnafu = sum . zipWith (*) powersOfFive . map snafuDigit . reverse

snafuDigit :: Char -> Int
snafuDigit '2' = 2
snafuDigit '1' = 1
snafuDigit '0' = 0
snafuDigit '-' = -1
snafuDigit '=' = -2
snafuDigit e = error ("Bad snafu number: " ++ [e])

powersOfFive :: [Int]
powersOfFive = iterate (*5) 1

convertToSnafu :: Int -> String
convertToSnafu x = reverse $ convertDigitToSnafu 0 x

convertDigitToSnafu :: Int -> Int -> String
convertDigitToSnafu 0 0 = []
convertDigitToSnafu 1 0 = "1"
convertDigitToSnafu carry x =
    let m = x `mod` 5
        x' = x `div` 5
        (carry', c) = digitSnafu (m + carry)
    in  c : convertDigitToSnafu carry' x'

digitSnafu :: Int -> (Int, Char)
digitSnafu 0 = (0, '0')
digitSnafu 1 = (0, '1')
digitSnafu 2 = (0, '2')
digitSnafu 3 = (1, '=')
digitSnafu 4 = (1, '-')
digitSnafu 5 = (1, '0')
digitSnafu e = error ("Invalid SNAFU digit: " ++ show e)