module Lib where
import Data.Maybe
import Data.List
import Data.List.Index

data Cell = Alive | Dead deriving (Eq)
instance Show Cell where
    show Alive = "O" 
    show Dead = "_" 

type Grid = [Line]
showGrid lines = intercalate "\n" (map showLine lines)

type Line = [Cell]
showLine cells = foldl (++) "" (map show cells)

neighborhood :: Grid -> Int -> Int -> [Cell]
neighborhood grid x y = let line = (grid!!(x)) 
                           in catMaybes [(left line y), (Just (line!!y)),  (right line y)] 

upperNeighbors ::  Grid -> Int -> Int -> [Cell]
upperNeighbors _ 0 _ = []
upperNeighbors grid x y = neighborhood grid (x-1) y

lowerNeighbors ::  Grid -> Int -> Int -> [Cell]
lowerNeighbors grid y _ | y+1 >= (length grid) = []
lowerNeighbors grid x y = neighborhood grid (x+1) y

sameLineNeighbors :: Grid -> Int -> Int -> [Cell]
sameLineNeighbors grid x y = let line = (grid!!x) 
                in (catMaybes [(left line y), (right line y)])

neighbors :: Grid -> Int -> Int -> [Cell]
neighbors grid x y = (upperNeighbors grid x y) 
                                ++ (sameLineNeighbors grid x y) 
                                ++ (lowerNeighbors grid x y)

countNeighbors :: Grid -> Int -> Int -> Int
countNeighbors grid x y = let neighborCells = neighbors grid x y
                            in (length.(filter isAlive)) neighborCells

nextCellState :: Cell -> Int -> Cell
nextCellState Alive 2 = Alive
nextCellState _ 3 = Alive
nextCellState _ _ = Dead

nextCellGen :: Grid -> Int -> Int -> Cell -> Cell
nextCellGen grid x y cell  = nextCellState cell (countNeighbors grid x y) 

nextLineGen :: Grid -> Int -> Line -> Line
nextLineGen grid x line = imap (nextCellGen grid x) line

nextGen :: Grid -> Grid
nextGen grid = imap (nextLineGen grid) grid

gameOfLife :: String
gameOfLife = show([[Alive, Dead, Alive], [Alive, Dead, Alive], [Alive, Dead, Alive]])

takeFrom :: Int -> Int -> [a] -> [a]
takeFrom start stop = (take stop).(drop start)

left :: [a] -> Int -> Maybe a
left list 0 = Nothing
left list index = let leftIndex = index-1
                    in Just (list!!leftIndex)

right :: [a] -> Int -> Maybe a
right list index | index+1 >= (length list) = Nothing
right list index = let leftIndex = index+1
                    in Just (list!!leftIndex)

isAlive :: Cell -> Bool
isAlive Alive = True
isAlive Dead = False