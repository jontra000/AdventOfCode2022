module Main (main) where

-- import Data.Time.Clock (getCurrentTime, utctDayTime)
import P11

main :: IO ()
main = do
    -- ts1 <-  getCurrentTime
    input <- readFile "inputs/input11"
    let result = run2 input
    print result
    -- ts2 <- getCurrentTime
    -- print (utctDayTime ts2 - utctDayTime ts1)
