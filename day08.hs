module Day08 where

import System.IO
import Data.Char
import Data.List
import Data.List.Split

filename = "inputs/input08.txt"

dirVisible :: Int -> [Int] -> Int
dirVisible tree line = fromEnum $ all (< tree) line

dirScore :: Int -> [Int] -> Int
dirScore tree line = let trees = (\x-> (fst (splitAt 1 (snd x))) ++ fst x) (break (>= tree) line)
                     in length trees

getDirs :: (Int, Int) -> [[Int]] -> [[Int]]
getDirs coords field = let x = fst coords
                           y = snd coords
                           right = drop (y + 1) $ field !! x
                           down = drop (x + 1) $ (transpose field) !! y
                           left = reverse $ take y $ field !! x
                           up = reverse $ take x $ (transpose field) !! y
                       in [right, down, left, up]

iterDirs :: (Int, Int) -> [[Int]] -> (Int -> [Int] -> Int) -> [Int]
iterDirs coords field fn = let x = fst coords
                               y = snd coords
                               dirs = getDirs coords field
                           in map (fn (field !! x !! y)) dirs

iterField :: [[Int]] -> (Int -> [Int] -> Int) -> [[Int]]
iterField field fn = let size = length field
                         all_cords = [(x, y) | x <- take size [0..], y <- take size [0..]]
                     in map (\x -> iterDirs x field fn) all_cords

parseInput :: String -> [[Int]]
parseInput x = map (map digitToInt) $ lines x

main :: IO ()
main = do
    input <- readFile filename
    let parsed = parseInput input
        part1 = length $ filter (>0) $ map sum $ iterField parsed dirVisible
        part2 = maximum $ filter (>0) $ map product $ iterField parsed dirScore
    putStr $ show part1 <> " " <> show part2
