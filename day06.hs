module Day06 where

import Data.List.Unique
import System.IO

filename = "inputs/input06.txt"

findMarker :: Int -> Int -> String -> Int
findMarker size n xs = if allUnique $ take size xs
                       then n
                       else findMarker size (n+1) (tail xs)

main :: IO ()
main = do
    input <- readFile filename
    let part1 = findMarker 4 4 input
        part2 = findMarker 14 14 input
    putStr $ show part1 <> " " <> show part2