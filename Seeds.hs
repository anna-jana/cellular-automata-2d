module Seeds where

import CellularAutomata2D
import GUI

main :: IO ()
main = runCellularAutomata2D seedsRule s3 >> return ()

s1, s2 :: Torus Bool
s1 = initBoolSpaceWithCells (40, 40) [(0,1),(1,1)]
s2 = initBoolSpaceWithCells (40, 40) [(0,1),(1,0),(0,0),(1,1)]
s3 = initBoolSpaceWithCells (40, 40) [(0,1),(1,0),(0,0)]

seedsRule :: Rule Bool
seedsRule = makeTotalMoorRule [2] []
