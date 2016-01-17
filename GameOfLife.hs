module GameOfLife where

import CellularAutomata2D
import GUI

main :: IO ()
main = gol glieder

gol :: Torus Int -> IO ()
gol space = runCellularAutomata2D space  golRule

glieder :: Torus Int
glieder = initIntSpaceWithCells (20, 20)
    (zip [(0,2),(1,2),(2,2),(2,1),(1,0)] (repeat 1))

golRule :: Rule Int
golRule = makeTotalMoorRule [2,3] [3]

instance Cell Int where
    getColor = ([white, black] !!)
    getSuccState 0 = 1
    getSuccState 1 = 0
