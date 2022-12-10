module Day05 where

import Control.Lens
import Data.List
import Data.List.Split
import System.IO

filename = "inputs/input05.txt"

stacks = [
    ["N", "H", "S", "J", "F", "W", "T", "D"],
    ["G", "B", "N", "T", "Q", "P", "R", "H"],
    ["V", "Q", "L"],
    ["Q", "R", "W", "S", "B", "N"],
    ["B", "M", "V", "T", "F", "D", "N"],
    ["R", "T", "H", "V", "B", "D", "M"],
    ["J", "Q", "B", "D"],
    ["Q", "H", "Z", "R", "V", "J", "N", "D"],
    ["S", "M", "H", "N", "B"]
    ]

move :: Int -> Int -> Int -> [[String]] -> [[String]]
move c a b xs = let pieces = take c (xs !! a)
                    from = drop c (xs !! a)
                    to = pieces ++ (xs !! b)
                    in (xs & element a .~ from) & element b .~ to

moveIter :: Int -> Int -> Int -> [[String]] -> [[String]]
moveIter c a b xs = iterate (move 1 a b) xs !! c

step1 :: [[String]] -> [String] -> [[String]]
step1 acc x = let c = read (x !! 1) :: Int
                  a = (read (x !! 3)) - 1
                  b = (read (x !! 5)) - 1
              in moveIter c a b acc

step2 :: [[String]] -> [String] -> [[String]]
step2 acc x = let c = read (x !! 1) :: Int
                  a = (read (x !! 3)) - 1
                  b = (read (x !! 5)) - 1
              in move c a b acc

main :: IO ()
main = do
    input <- readFile filename
    let moves = [words x | x <- (lines input)]
        part1 = intercalate "" [head x | x <- foldl step1 stacks moves]
        part2 = intercalate "" [head x | x <- foldl step2 stacks moves]
    putStr $ show part1 <> " " <> show part2