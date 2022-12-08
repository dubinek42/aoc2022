module Day03 where

import System.IO
import Data.Char
import Data.List
import Data.List.Split

filename = "inputs/input03.txt"

priority :: Char -> Int
priority c
    | c `elem` ['a'..'z'] = ord c - 96
    | c `elem` ['A'..'Z'] = ord c - 64 + 26
    | otherwise           = 0

intersect' :: (String, String) -> Char
intersect' x = head $ intersect (fst x) (snd x)

intersect'' :: [String] -> Char
intersect'' [x, y, z] = head $ intersect x (intersect y z)

rucksack :: String -> Int
rucksack x = let s = splitAt (length x `div` 2) x
             in priority (intersect' s)

groups :: [String] -> Int
groups x = priority $ intersect'' x

main :: IO ()
main = do
    input <- readFile filename
    let part1 = sum $ map rucksack $ lines input
        part2 = sum $ map groups $ chunksOf 3 $ lines input
    putStr $ show part1 <> " " <> show part2