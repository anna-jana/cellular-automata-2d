module GameOfLife where

import CellularAutomata2D

gol :: Space Int -> IO ()
gol space = runCellularAutomata2D space [0,1] ([black, white] !!)
    (makeTotalMoorRule [2,3] [3])

glieder = initSpace 20 20 (zip [(0,2),(1,2),(2,2),(2,1),(1,0)] (repeat 1))
