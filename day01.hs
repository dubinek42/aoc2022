module Day01 where

import System.IO
import Data.List
import Data.List.Split

filename = "inputs/input01.txt"

main :: IO ()
main = do
    input <- readFile filename
    let part1 = calories1 (parseInput input)
        part2 = calories2 (parseInput input)
    putStr $ show part1 <> " " <> show part2

parseInput :: String -> [Int]
parseInput s = map (sum . map read) $ splitOn [""] $ lines s

calories1 :: [Int] -> Int  
calories1 xs = maximum xs

calories2 :: [Int] -> Int
calories2 xs = sum $ take 3 $ reverse $ sort xs
