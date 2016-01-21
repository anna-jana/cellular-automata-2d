-- | The famous totalistic cellular automata from John H. Conway.
module GameOfLife where

import CellularAutomata2D
import GUI

-- | Run a simple glider simulation.
main :: IO ()
main = runCellularAutomata2D golRule glider

glider :: Torus Int
glider = initIntSpaceWithCells (20, 20)
    (zip [(0,2),(1,2),(2,2),(2,1),(1,0)] (repeat 1))

-- | If a cell is alive its dies from overpopulation if its has more than tree neighbors
--   or from starvation if it has fewer than 2 neighbors.
--   Otherwise the cell stays alive.
--   A new cell is born on an empty cell if is has exactly two neighbors.
--   Otherwise the cell stays dead.
--   The game of life uses a moor neighborhood.
golRule :: Rule Int
golRule = makeTotalMoorRule [2,3] [3]

instance Cell Int where
    getColor = ([white, black] !!)
    getSuccState 0 = 1
    getSuccState 1 = 0
