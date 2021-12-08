module Day5 where

import qualified Data.Map.Lazy as Map

main :: IO ()
main = do
  contents <- readFile "inputs/5.txt"
  let ls = lines contents
  mapM_ (putStrLn . show) (take 5 ls)
  
data Pos = From (Int, Int) | To (Int,Int)


