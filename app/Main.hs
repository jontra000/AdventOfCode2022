module Main (main) where

import P12

main :: IO ()
main = do
    input <- readFile inputLocation
    let result1 = run1 input
        result2 = run2 input
    print result1
    print result2
