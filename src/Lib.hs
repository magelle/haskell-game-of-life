module Lib where
import Data.Maybe
import Data.List
import Data.List.Index

data Cell = Alive | Dead deriving (Eq)
instance Show Cell where
    show Alive = "O" 
    show Dead = "_" 

isAlive :: Cell -> Bool
isAlive Alive = True
isAlive Dead = False

type Grid = [Line]
showGrid lines = intercalate "\n" (map showLine lines)

type Line = [Cell]
showLine cells = foldl (++) "" (map show cells)

neighbors :: [[a]] -> Int -> Int -> [a]
neighbors grid x y = map (get grid) (removeOutside grid (neighborsCoord (x, y)))

get :: [[a]] -> (Int, Int) -> a
get grid (x, y) = (grid!!x)!!y

neighborsCoord :: (Int, Int) -> [(Int, Int)]
neighborsCoord (x, y) = filter ((x, y) /=) [(i,j) | i <- [(x-1) .. (x+1)],
                                                    j <- [(y-1) .. (y+1)]]

removeOutside :: [[a]] -> [(Int, Int)] -> [(Int, Int)]
removeOutside grid coords = filter (isOutside grid) coords

isOutside :: [[a]] -> (Int, Int) -> Bool
isOutside grid (x, y) = x >=0 
                     && x < (length grid) 
                     && y >= 0 
                     && y < (length (grid!!0))

nextCellState :: Cell -> Int -> Cell
nextCellState Alive 2 = Alive
nextCellState _ 3 = Alive
nextCellState _ _ = Dead

countNeighbors :: Grid -> Int -> Int -> Int
countNeighbors grid x y = let neighborCells = neighbors grid x y
                            in (length.(filter isAlive)) neighborCells

nextCellGen :: Grid -> Int -> Int -> Cell -> Cell
nextCellGen grid x y cell  = nextCellState cell (countNeighbors grid x y) 

nextLineGen :: Grid -> Int -> Line -> Line
nextLineGen grid x line = imap (nextCellGen grid x) line

nextGen :: Grid -> Grid
nextGen grid = imap (nextLineGen grid) grid

gameOfLife :: String
gameOfLife = show([[Alive, Dead, Alive], [Alive, Dead, Alive], [Alive, Dead, Alive]])
