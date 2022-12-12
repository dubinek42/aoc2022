module Day11 where

import Data.List
import Data.List.Split

filename = "inputs/input11.txt"

data Monkeys = Monkeys { ops :: [(Int -> Int)]
                       , divs :: [Int]
                       , tests :: [(Int -> Int)]
                       }

parseItems :: String -> [Int]
parseItems xs = let itemLine = (lines $ xs) !! 1
                    itemString = drop 18 itemLine
                in [read x | x <- (splitOn ", " itemString)]

eval' :: String -> (Int -> Int)
eval' "* old" = (^2)
eval' ('*':xs) = (*) (read xs :: Int)
eval' ('+':xs) = (+) (read xs :: Int)

parseOp :: String -> (Int -> Int)
parseOp xs = let itemLine = (lines $ xs) !! 2
             in eval' $ drop 23 itemLine

parseDiv :: String -> Int
parseDiv xs = let itemLine = (lines $ xs) !! 3
              in read $ drop 21 itemLine

parseTest :: String -> (Int -> Int)
parseTest xs = let d = parseDiv xs
                   t = read $ drop 29 $ (lines $ xs) !! 4
                   f = read $ drop 30 $ (lines $ xs) !! 5
               in (\x -> if (mod x d) == 0 then t else f)

passItem :: [[Int]] -> (Int, Int) -> [[Int]]
passItem is (i, p) = let before = take p is
                         x = is !! p
                         after = drop (p+1) is
                     in before ++ [x ++ [i]] ++ after

deleteFrom :: Int -> [[Int]] -> [[Int]]
deleteFrom id xs = let before = take id xs
                       after = drop (id+1) xs
                   in before ++ [[]] ++ after

addInspections :: Int -> Int -> [Int] -> [Int]
addInspections n id xs = let before = take id xs
                             x = (xs !! id) + n
                             after = drop (id+1) xs
                         in before ++ [x] ++ after


doMonkey :: Int -> (Int -> Int) -> Monkeys -> ([[Int]], [Int]) -> ([[Int]], [Int])
doMonkey id relief m (is, ins) = let i = is !! id
                                     op = (ops m) !! id
                                     test = (tests m) !! id
                                     oped = map relief $ map op i
                                     passTo = map test oped
                                     newItems = foldl passItem (deleteFrom id is) (zip oped passTo)
                                     newInspections = addInspections (length i) id ins
                                 in (newItems, newInspections)

doAllMs :: (Int -> Int) -> Monkeys -> ([[Int]], [Int]) -> ([[Int]], [Int])
doAllMs relief m (is, ins) = foldl (\acc x -> doMonkey x relief m acc) (is, ins) [0..(length is)-1]

lcm' :: [Int] -> Int
lcm' [] = 1
lcm' (x:xs) = lcm x (lcm' xs)

main :: IO ()
main = do
    input <- readFile filename
    let monkeys = splitOn "\n\n" $ input
        items = map parseItems monkeys
        ops = map parseOp monkeys
        divs = map parseDiv monkeys
        tests = map parseTest monkeys
        inspections = replicate (length items) 0
        m = Monkeys {ops=ops, divs=divs, tests=tests}
        part1Ins = snd $ foldl (\acc _ -> doAllMs (`div` 3) m acc) (items, inspections) [0..19]
        part2Ins = snd $ foldl (\acc _ -> doAllMs (\x -> mod x (lcm' divs)) m acc) (items, inspections) [0..9999]
        part1 = product $ take 2 $ reverse $ sort $ part1Ins
        part2 = product $ take 2 $ reverse $ sort $ part2Ins
    putStr $ show part1 <> " " <> show part2