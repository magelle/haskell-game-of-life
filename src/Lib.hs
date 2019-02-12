module Lib where

import Data.List

data Cell = Alive | Dead deriving (Eq)
instance Show Cell where
    show Alive = "O" 
    show Dead = "_" 

data Grid = Grid [Line]
instance Show Grid where
    show (Grid lines) = intercalate "\n" (map show lines)

data Line = Line [Cell]
instance Show Line where
    show (Line cells) = foldl (++) "" (map show cells)

nextGen :: Cell -> Int -> Cell
nextGen Alive 2 = Alive
nextGen _ 3 = Alive
nextGen _ _ = Dead

gameOfLife :: String
gameOfLife = show(Grid([Line([Alive, Dead, Alive]), Line([Alive, Dead, Alive]), Line([Alive, Dead, Alive])]))