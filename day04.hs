module Day08 where

import Data.List
import Data.List.Split
import System.IO

filename = "inputs/input04.txt"

parseLine :: String -> [[String]]
parseLine x = map (splitOn "-") $ splitOn "," x

createRange :: [String] -> [Int]
createRange [x,y] = [(read x :: Int)..(read y :: Int)]

createRanges :: [[String]] -> ([Int], [Int])
createRanges [x, y] = (createRange x, createRange y)

contained :: ([Int], [Int]) -> Int
contained t = let x = fst t
                  y = snd t
              in fromEnum $ isInfixOf x y || isInfixOf y x

overlap :: ([Int], [Int]) -> Int
overlap t = let i = intersect (fst t) (snd t)
            in fromEnum $ (length i) > 0

main :: IO ()
main = do
    input <- readFile filename
    let xs = map createRanges $ map parseLine $ lines input
        part1 = sum $ map contained xs
        part2 = sum $ map overlap xs
    putStr $ show part1 <> " " <> show part2