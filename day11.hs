module Day11 where
import FileReader
import Data.List
import Data.Char
import Debug.Trace
import Control.Monad.State

main :: Int -> IO ()
main n = do
  ls <- readF 11
  let ms = map (map digitToInt) ls
  eval n ms

type Matrix = [[Int]]
type Pos = (Int, Int)

example :: Matrix
example = [
  [1,1,1,1,1],
  [1,9,9,9,1],
  [1,9,1,9,1],
  [1,9,9,9,1],
  [1,1,1,1,1]
  ]

step1 :: Matrix
step1 = [[2,2,2,2,2],[2,10,10,10,2],[2,10,2,10,2],[2,10,10,10,2],[2,2,2,2,2]]

eval :: Int -> Matrix -> IO ()
eval n mx = do
  -- putStrLn $ "Initial State:" 
  -- putStrLn $ str (toStrArr mx)
  let (res, v) = execState (run n) (mx,0)
  putStrLn $ "After " ++ show n ++ " steps: "
  putStrLn $ str (toStrArr res)
  putStrLn $ "Total of " ++ show v ++ " flashes"

type StateM a = State (Matrix,Int) a
startState = (example, 0)

run :: Int -> StateM Int
run 0 = do
  (mx, v) <- get
  -- traceM (str (toStrArr mx))
  return v

run n = do
  (mx, v) <- get
  traceM (str (toStrArr mx))
  put (tick mx, v)
  ripple
  run (n-1)

toStrArr mx = map unwords (map (map show) mx)
str [] = ""
str (x:xs) = x ++ "\n" ++ str xs

tick :: Matrix -> Matrix
tick mx = map (map (+1)) mx

ripple :: StateM (Matrix, Int)
ripple = do
  (mx, v) <- get
  let end = (length (head mx), length mx)
  -- traceShowM end
  let ps = findFlashes mx
  let adjs = fInvalid end (concat (map adjPos ps))
  case adjs of 
    [] -> get
    _  -> do
      mx' <- incPos adjs
      let rs = reset mx'
      put (rs, v + count rs)
      ripple

incPos :: [Pos] -> StateM Matrix
incPos [] = get >>= \(mx, _) -> return mx
incPos ((x,y):ps) = do
  (mx, v) <- get
  let val = (mx !! y) !! x
  -- traceShowM val
  let mx' = insertM (x,y) (val+1) mx
  -- traceM (str (toStrArr (reset mx'))) 
  put (mx', v)
  incPos ps

count :: Matrix -> Int
count = length . filter (==0) . concat
  
insertAt :: Int -> a -> [a] -> [a]
insertAt i e xs = take i xs ++ [e] ++ drop (i+1) xs

insertM :: Pos -> Int -> Matrix -> Matrix
insertM (x,y) n mx = insertAt y (insertAt x n (mx !! y)) mx

reset :: Matrix -> Matrix
reset mx = [[if x > 9 then 0 else x | x <- l] | l <- mx]

findFlashes :: Matrix -> [Pos]
findFlashes [] = []
findFlashes mx = concat $ map (\y -> ffRow (mx !! y) y) [0..(length mx - 1)] 

ffRow :: [Int] -> Int -> [Pos]
ffRow rs y = filter (\x -> x /= (-1,-1)) ff
  where ff = [if (rs !! a) > 9 then (a,y) else (-1,-1) | a <- [0..(length rs - 1)]]

adjPos :: Pos -> [Pos]
adjPos (x,y) = [
  (x-1,y-1),(x,y-1),(x+1,y-1),
  (x-1,y),(x+1,y),
  (x-1,y+1),(x,y+1),(x+1,y+1)]

fInvalid :: Pos -> [Pos] -> [Pos]
fInvalid _    []     = []
fInvalid end@(endX,endY) ((x,y):ps)
  | x < endX && x >= 0 && y < endY && y >= 0 = (x,y) : fInvalid end ps
  | otherwise            = fInvalid end ps
