module Day01 where

import System.IO
import Data.List
import Data.List.Split

filename = "inputs/input01.txt"

main :: IO ()
main = do
    input <- readFile filename
    let part1 = elves1 input
        part2 = elves2 input
    putStr $ show part1 <> " " <> show part2

elves1 :: String -> Int  
elves1 s = maximum $ map (sum . map read) $ splitOn [""] $ lines s

elves2 :: String -> Int
elves2 s = sum $ take 3 $ reverse $ sort $ map (sum . map read) $ splitOn [""] $ lines s
