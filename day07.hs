module Day07 where

import System.IO
import Data.List
import qualified Data.Map as Map 

filename = "inputs/input07.txt"

turn :: (Map.Map String Int, [String]) -> String -> (Map.Map String Int, [String])
turn (fs, crnt) cmnd =  if isPrefixOf "$ cd" cmnd
                            then let path = cd crnt cmnd
                                 in (fs, path)
                            else if (not (isPrefixOf "$ ls" cmnd)) &&
                                    (not (isPrefixOf "dir " cmnd))
                                then let newFilesystem = saveAll crnt cmnd fs
                                     in (newFilesystem, crnt)
                                else (fs, crnt)

cd :: [String] -> String -> [String]
cd crnt cmnd = let dirName = last (words cmnd)
                     in if dirName == ".."
                        then tail crnt
                        else dirName : crnt

hashPath :: [String] -> String
hashPath [] = "/"
hashPath xs =  intercalate "/" xs

saveSize :: String -> String -> Map.Map String Int -> Map.Map String Int
saveSize x p xs = let s = read (head (words x)) :: Int
                  in if Map.member p xs
                     then Map.update (\y -> Just (y + s)) p xs
                     else Map.insert p s xs

saveAll :: [String] -> String -> Map.Map String Int -> Map.Map String Int
saveAll [] x xs = xs
saveAll (y:[]) x xs = saveSize x (hashPath [y]) xs
saveAll ys x xs = saveSize x (hashPath ys) (saveAll (tail ys) x xs)

main :: IO ()
main = do
    input <- readFile filename
    let fs = fst $ foldl turn ((Map.singleton "/" 0), ["/"]) $ lines input
        spaceNeeded = 30000000 - (70000000 - (fs Map.! "/"))
        sizes = Map.elems fs
        part1 = sum $ filter (<= 100000) sizes
        part2 = minimum $ filter (>= spaceNeeded) sizes
    putStr $ show part1 <> " " <> show part2
