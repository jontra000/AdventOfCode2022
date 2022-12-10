module Main (main) where

-- import Data.Time.Clock (getCurrentTime, utctDayTime)
import P10

main :: IO ()
main = do
    -- ts1 <-  getCurrentTime
    input <- readFile "inputs/input10"
    let result = run2 input
    print result
    -- ts2 <- getCurrentTime
    -- print (utctDayTime ts2 - utctDayTime ts1)
