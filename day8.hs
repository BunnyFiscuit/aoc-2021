module Day8 where
import System.IO
import Data.List (sort)
import Debug.Trace

main :: IO ()
main = do
  contents <- readFile "inputs/8.txt"
  let ls = lines contents
  let ws = map (str2Entry . words) ls
  let os = map snd ws
  putStrLn $ show os
  -- let ez = map (number . sort) os
  -- putStrLn $ show ez
  
type Entry = ([String], [String])

str2Entry :: [String] -> Entry
str2Entry s = (input,output)
  where input  = takeWhile (/="|") s
        output = drop 1 $ dropWhile (/="|") s 
        
easyNumbers :: String -> Bool
easyNumbers s = l == 2 || l == 3 || l == 4 || l == 7
  where l = length s

validStr s = any (==s) ["ab", ""]

number :: String -> Int
number "abcdeg"  = 0
number "ab"      = 1 
number "acdfg"   = 2 
number "abcdf"   = 3
number "abef"    = 4
number "bcdef"   = 5
number "bcdefg"  = 6
number "abd"     = 7 
number "abcdefg" = 8
number "abcdef"  = 9
number s         = error $ "Not a valid str " ++ s
