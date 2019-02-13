module Lib where

import Data.List

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

neighborhood :: Int -> [Cell] -> [Cell]
neighborhood target = takeFrom (target - 1) 3 

removeMiddle :: [a] -> [a]
removeMiddle (a:b:tail) = a:tail

neighbors :: Int -> Line -> [Cell]
neighbors target (Line cells) = (removeMiddle.(neighborhood target)) cells

countNeighbors :: Grid -> Int -> Int -> Int
countNeighbors (Grid ((Line (Dead:_)):_)) x y = 0
countNeighbors (Grid ((Line (Alive:_)):_)) x y = 1

nextCellState :: Cell -> Int -> Cell
nextCellState Alive 2 = Alive
nextCellState _ 3 = Alive
nextCellState _ _ = Dead

nextGen :: Grid -> Grid
nextGen grid = grid

gameOfLife :: String
gameOfLife = show(Grid([Line([Alive, Dead, Alive]), Line([Alive, Dead, Alive]), Line([Alive, Dead, Alive])]))

takeFrom :: Int -> Int -> [a] -> [a]
takeFrom start stop = (take stop).(drop start)