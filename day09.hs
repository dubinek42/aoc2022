module Day09 where

import qualified Data.Set as Set

filename = "inputs/input09.txt"

type Coord = (Int, Int)
type Params = ([Coord], [Set.Set Coord])

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

follow' :: [Coord] -> Int -> [Coord]
follow' xs n = let before = take (n) xs
                   new = follow (xs !! (n-1)) (xs !! n)
                   after = drop (n+1) xs
               in before ++ [new] ++ after

move' :: Params -> String -> Params
move' ((x:xs), vs) d = let moved = (move d x) : xs
                           followed = foldl (\acc x -> follow' acc x) moved [1..(length moved)-1]
                           vs1 = Set.insert (head followed) (head vs)
                           vs9 = Set.insert (last followed) (last vs)
                        in (followed, [vs1, vs9])

iterMove' :: Params -> String -> Int -> Params
iterMove' params d n = iterate (\x -> move' x d) params !! n

main :: IO ()
main = do
    input <- readFile filename
    let moves = map (\x -> ((head x), (read (last x) :: Int))) $ map words $ lines input
        paramsFn n = (replicate n (0,0), replicate 2 Set.empty)
        foldFn = (\acc x -> iterMove' acc (fst x) (snd x))
        part1 = length $ last $ snd $ foldl foldFn (paramsFn 2) moves
        part2 = length $ last $ snd $ foldl foldFn (paramsFn 10) moves
    putStr $ show part1 <> " " <> show part2
