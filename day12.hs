module Day12 where
import System.IO
import FileReader
import Data.Char
import Data.List
import Control.Monad.State

main :: IO ()
main = do
  contents <- readF 12
  -- putStrLn $ show contents
  let ps = parseStr contents
  putStrLn $ show ps


data MRecord = MRecord { 
  edges :: Edges,
  paths :: [Path]  
} deriving (Show)

type EState a = State MRecord a

data Node  = Start | End | Bg String | Sm String deriving (Show, Eq)
type Edge  = (Node, Node) 
type MultiEdge = (Node, [Node])
type Edges = [Edge]
type Path  = [Node]


parseStr :: [String] -> [Edge]
parseStr [] = []
parseStr (x:xs) = (str2Node start, str2Node end) : parseStr xs
  where start = takeWhile (/='-') x
        end   = drop 1 (dropWhile(/='-') x)

str2Node :: String -> Node
str2Node "start" = Start
str2Node "end"   = End
str2Node x       = toCave x

toCave :: String -> Node
toCave x = if isBigCave x then Bg x else Sm x

isBigCave :: String -> Bool
isBigCave xs = and (map isUpper xs)

correctEdges :: Edges -> Edges
correctEdges [] = []
correctEdges ((e,Start):es) = (Start,e) : correctEdges es
correctEdges ((End,e)  :es) = (e,End) : correctEdges es
correctEdges (e:es)             = e : correctEdges es 

startEdges :: Edges -> Edges
startEdges es = filter isStartEdge es

isStartEdge :: Edge -> Bool
isStartEdge (Start,_) = True
isStartEdge _         = False

groupEdges :: Edges -> [Edges]
groupEdges = groupBy (\x y -> fst x == fst y)

makeMultiEdges :: [Edges] -> [MultiEdge]
makeMultiEdges = undefined