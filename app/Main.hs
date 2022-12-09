module Main (main) where

-- import Data.Time.Clock (getCurrentTime, utctDayTime)
import P8

main :: IO ()
main = do
    -- ts1 <-  getCurrentTime
    input <- readFile "inputs/input8"
    let result = run2 input
    print result
    -- ts2 <- getCurrentTime
    -- print (utctDayTime ts2 - utctDayTime ts1)
