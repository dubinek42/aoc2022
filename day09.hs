module Day09 where

import qualified Data.Set as Set

filename = "inputs/input09.txt"

type Coord = (Int, Int)

move :: String -> Coord -> Coord
move "R" (x, y) = (x+1, y)
move "D" (x, y) = (x, y-1)
move "L" (x, y) = (x-1, y)
move "U" (x, y) = (x, y+1)

follow :: Coord -> Coord -> Coord
follow (x, y) (a, b) = let xa = x - a
                           yb = y - b
                           a'
                            | abs xa > 1 = a + signum xa
                            | abs xa == 1 && abs yb > 1 = a + signum xa
                            | otherwise = a
                           b'
                            | abs yb > 1 = b + signum yb
                            | abs yb == 1 && abs xa > 1 = b + signum yb
                            | otherwise = b
                       in (a', b')

doMove :: (Coord, Coord, Set.Set Coord) -> String -> (Coord, Coord, Set.Set Coord)
doMove (head, tail, vs) d = let newHead = move d head
                                newTail = follow newHead tail
                                newVs = Set.insert newTail vs
                            in (newHead, newTail, newVs)

iterMove :: (Coord, Coord, Set.Set Coord) -> String -> Int -> (Coord, Coord, Set.Set Coord)
iterMove params d n = iterate (\x -> doMove x d) params !! n 

get3rd :: (a, b, c) -> c
get3rd (_,_,a) = a

main :: IO ()
main = do
    input <- readFile filename
    let moves = map (\x -> ((head x), (read (last x) :: Int))) $ map words $ lines input
        go1 = foldl (\acc x -> iterMove acc (fst x) (snd x)) ((0,0), (0,0), Set.empty) moves
        part1 = length $ get3rd go1
    print $ part1
