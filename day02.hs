module Day02 where

import System.IO
import Data.List.Split

filename = "inputs/input02.txt"

shapes = [("A", 0), ("B", 1), ("C", 2),
          ("X", 0), ("Y", 1), ("Z", 2)]

score :: [String] -> (Int -> Int -> Int) -> Int
score l fn = 
    let x = (\(Just i) -> i) (lookup (head l) shapes)
        y = (\(Just i) -> i) (lookup (last l) shapes)
    in fn x y

score1 :: Int -> Int -> Int
score1 x y = y + 1 + mod (y - x + 1) 3 * 3

score2 :: Int -> Int -> Int
score2 x y = mod (x + y - 1) 3 + 1 + y * 3

parseInput :: String -> [[String]]
parseInput x = map words (lines x)

main :: IO ()
main = do
    input <- readFile filename
    let parsed = parseInput input
        part1 = sum $ map (\x -> score x score1) parsed
        part2 = sum $ map (\x -> score x score2) parsed
    putStr $ show part1 <> " " <> show part2