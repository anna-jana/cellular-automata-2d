module GameOfLife where

import CellularAutomata2D
import GUI
import Term

main :: IO ()
main = gol glieder -- runInTerminal (("#_"!!) . snd) glieder golRule

gol :: Space s => s (Int, Int) -> IO ()
gol space = runCellularAutomata2D space [(0,0),(0,1)] (([black, white] !!) . snd) golRule

glieder :: Torus (Int, Int)
glieder = initSpaceWithCells (20, 20) (0, 0)
    (zip [(0,2),(1,2),(2,2),(2,1),(1,0)] (repeat (0,1)))

golRule :: Rule (Int, Int)
golRule = makeReversibleRule (makeTotalMoorRule [2,3] [3])
