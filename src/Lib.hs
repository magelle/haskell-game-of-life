module Lib where
import Data.Maybe
import Data.List
import Data.List.Index

data Cell = Alive | Dead deriving (Eq)
instance Show Cell where
    show Alive = "O" 
    show Dead = "_" 

data Grid = Grid [Line] deriving (Eq)
instance Show Grid where
    show (Grid lines) = intercalate "\n" (map show lines)

data Line = Line [Cell] deriving (Eq)
instance Show Line where
    show (Line cells) = foldl (++) "" (map show cells)

neighborhood :: [Line] -> Int -> Int -> [Cell]
neighborhood lines x y = let (Line cells) = (lines!!(x)) 
                           in catMaybes [(left cells y), (Just (cells!!y)),  (right cells y)] 

upperNeighbors ::  [Line] -> Int -> Int -> [Cell]
upperNeighbors _ 0 _ = []
upperNeighbors lines x y = neighborhood lines (x-1) y

lowerNeighbors ::  [Line] -> Int -> Int -> [Cell]
lowerNeighbors lines y _ | y+1 >= (length lines) = []
lowerNeighbors lines x y = neighborhood lines (x+1) y

sameLineNeighbors :: [Line] -> Int -> Int -> [Cell]
sameLineNeighbors lines x y = let (Line cells) = (lines!!x) 
                in (catMaybes [(left cells y), (right cells y)])

neighbors :: Grid -> Int -> Int -> [Cell]
neighbors (Grid lines) x y = (upperNeighbors lines x y) 
                                ++ (sameLineNeighbors lines x y) 
                                ++ (lowerNeighbors lines x y)

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
nextLineGen grid x (Line cells) = Line (imap (nextCellGen grid x) cells)

nextGen :: Grid -> Grid
nextGen grid@(Grid lines)= Grid (imap (nextLineGen grid) lines)

gameOfLife :: String
gameOfLife = show(Grid([Line([Alive, Dead, Alive]), Line([Alive, Dead, Alive]), Line([Alive, Dead, Alive])]))

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