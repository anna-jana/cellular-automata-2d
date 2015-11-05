module GameOfLife where

import CellularAutomata2D
import GUI
import Term

main :: IO ()
main = gol glieder

termGOL :: Space s => s Int -> IO ()
termGOL space = runInTerminal ("_#"!!) space golRule

gol :: Space s => s Int -> IO ()
gol space = runCellularAutomata2D space [0,1] ([black, white] !!) golRule

glieder :: Torus Int
glieder = initIntSpaceWithCells (20, 20)
    (zip [(0,2),(1,2),(2,2),(2,1),(1,0)] (repeat 1))

golRule :: Rule Int
golRule = makeTotalMoorRule [2,3] [3]
