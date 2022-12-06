module Day01 where

import System.IO
import Data.List
import Data.List.Split

filename = "inputs/input01.txt"
  
elves1 = do  
    contents <- readFile filename
    return $ maximum $ map (sum . map read) $ splitOn [""] $ lines contents

elves2 = do  
    contents <- readFile filename
    return $ sum $ take 3 $ reverse $ sort $ map (sum . map read) $ splitOn [""] $ lines contents
