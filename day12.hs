module Day12 where

import Data.Char
import Data.Graph.AStar
import Data.List
import Data.Maybe
import qualified Data.HashSet as HS

filename = "inputs/input12.txt"

elemIndex2d :: Char -> [String] -> (Int, Int)
elemIndex2d a as = let x = (\(Just i)->i) $ findIndex (elem a) as
                       y = (\(Just i)->i) $ elemIndex a (as !! x)
                   in (x, y)

height :: Char -> Int
height 'S' = height 'a'
height 'E' = height 'z'
height x = ord x - 96

heightRule :: Char -> Char -> Bool
heightRule x y = if (height y) - (height x) <= 1
                 then True
                 else False

neighbors :: [String] -> (Char -> Char -> Bool) -> (Int, Int) -> (HS.HashSet (Int, Int))
neighbors hm rule (x, y) = let directions = [(1, 0), (-1, 0), (0, 1), (0, -1)]
                           in let coords = map (\(q, w) -> (q+x, w+y)) directions
                                  inBounds = filter (\(q, w) -> q>=0 && q<(length hm)
                                                     && w>=0 && w<(length (head hm))) coords
                                  ruled = filter (\(q, w) -> rule ((hm !! x) !! y) ((hm !! q) !! w)) inBounds
                              in HS.fromList ruled

distance :: (Int, Int) -> (Int, Int) -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

findAllStarts :: Char -> [String] -> [(Int, Int)]
findAllStarts x xs = let zipped = zip [0..(length xs)] xs
                         findStart i x xs = map (\a -> (i, a)) (elemIndices x xs)
                     in concat $ map (\(a,b)-> findStart a x b) zipped

main :: IO ()
main = do
    input <- readFile filename
    let 
        heightmap = lines $ input
        start = elemIndex2d 'S' heightmap
        end = elemIndex2d 'E' heightmap
        findPath = aStar                            -- A Star alg. partially applied (without starting point)
                   (neighbors heightmap heightRule) -- The graph = function from vertices to neighbors
                   ((\_ _ -> 1))                    -- Distance between neighbors = always 1
                   (distance end)                   -- Distance from goal
                   (== end)                         -- Predicate "is this goal?"
        allStarts = findAllStarts 'a' heightmap
        allPaths = map findPath allStarts
        part1 = length $ (\(Just i)->i) $ findPath start
        part2 = minimum $ map length $ catMaybes allPaths
    putStr $ show part1 <> " " <> show part2
