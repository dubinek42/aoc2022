module Day10 where

import System.IO
import Data.List

filename = "inputs/input10.txt"

addx :: Int -> Int -> [Int]
addx x a = [x, x+a]

noop :: Int -> [Int]
noop x = [x]

command :: Int -> String -> [Int]
command x c = let w = words c
              in if length w > 1
                 then addx x (read (last w) :: Int)
                 else noop x

doCycle :: [Int] -> String -> [Int]
doCycle acc x = acc ++ (command (last acc) x)

strengths :: [Int] -> [Int] -> Int
strengths h ids = sum [(h !! (i-1) * i) | i <- ids]

pixel :: Int -> Int -> String
pixel x y = let m = mod y 40
            in if x == m || x == (m-1) || x == (m+1) 
               then "#"
               else "."

enumerate :: [Int] -> [(Int, Int)]
enumerate x = zip [0..length x - 1] x

makeLines :: Int -> Char -> String -> String
makeLines 0 y xs = xs
makeLines n y [] = []
makeLines n y xs
 | length xs < n = xs
 | otherwise = take n xs ++ [y] ++ makeLines n y (drop n xs)

main :: IO ()
main = do
    input <- readFile filename
    let history = foldl doCycle [1] $ lines input
        cycles = [20, 60, 100, 140, 180, 220]
        crt = map (\x -> pixel (snd x) (fst x)) (enumerate history)
        part1 = strengths history cycles
        part2 = makeLines 40 '\n' $ intercalate "" crt
    print $ part1
    putStr $ part2