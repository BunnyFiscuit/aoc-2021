module Day1 where

import System.IO
import Data.List


main :: IO ()
main = do
  contents <- readFile "inputs/1-jp.txt"
  let d = map (\s -> read s :: Int) (lines contents)
  putStrLn $ "task 1.1: " ++ show (increase d)
  putStrLn $ "task 1.2: " ++ show ((increase . mGroup) d)

increase :: [Int] -> Int
increase [] = 0
increase [x] = 0
increase (x:y:ys) 
  | x < y = 1 + increase (y:ys)
  | otherwise = increase (y:ys)
-- 2
mGroup :: [Int] -> [Int]
mGroup [] = []
mGroup [x,y] = [x+y]
mGroup (x:y:z:xs) = [x+y+z] ++ mGroup (y:z:xs)