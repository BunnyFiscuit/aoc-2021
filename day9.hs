module Day9 where
import FileReader
import Data.List
import Data.Char

main :: IO ()
main = do
  ls <- readF 9
  print ls
  gs <- runFirst ls
  print gs
  
type Pos = (Int,Int)

runFirst :: [String] -> IO [Pos]
runFirst ls = do
  let end = (length (head ls), length ls)
  let points = genPoints end
  let res = map (\p -> lowest (getValue ls p) (adjVals ls p)) points
  let ps = zip points res
  let zs = zip res (concat ls)
  let filMap = map (snd) (filter fst zs)
  print (calcRiskLevel filMap)
  return $ map fst (filter snd ps)
  
runSecond :: [String] -> IO ()
runSecond = undefined

lowest :: Int -> [Int] -> Bool
lowest n ns = minimum ns > n

getValue :: [String] -> Pos -> Int
getValue xs (x,y) = digitToInt $ (xs !! y) !! x

adjPos :: Pos -> [Pos]
adjPos (x,y) = [(x-1,y),(x,y-1),(x,y+1),(x+1,y)]

adjVals :: [String] -> Pos -> [Int]
adjVals xs p = map (getValue xs) ajs
  where end = (length (head xs), length xs)
        ajs = filterInvalid end (adjPos p)

filterInvalid :: Pos -> [Pos] -> [Pos]
filterInvalid _    []     = []
filterInvalid end@(endX,endY) ((x,y):ps)
  | x < endX && x >= 0 && y < endY && y >= 0 = (x,y) : filterInvalid end ps
  | otherwise            = filterInvalid end ps
  
genPoints :: Pos -> [Pos]
genPoints (x,y) = [(a,b) | b <- [0..y-1], a <- [0..x-1]]

calcRiskLevel :: String -> Int
calcRiskLevel [] = 0
calcRiskLevel (x:xs) = 1 + digitToInt x + calcRiskLevel xs

basin :: Pos -> [Pos]
basin = undefined

filter9 :: [String] -> [Pos] -> [Pos]
filter9 xs ps = filter (\p -> getValue xs p /= 9) ps 









