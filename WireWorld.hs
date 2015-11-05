module WireWorld where

import CellularAutomata2D
import GUI

data Cell = Empty | Conductor | ElectronHead | ElectronTail
    deriving (Show, Eq, Enum, Bounded)

main :: IO ()
main = runCellularAutomata2D
    (initSpaceWithCells (50, 50) Empty [] :: Torus Cell)
    [minBound..maxBound]
    (([grey, yellow, blue, red] !!) . fromEnum)
    wireWorldRule

wireWorldRule :: Rule Cell
wireWorldRule = Rule moorIndexDeltas (\self friends -> return $ case self of
    Empty -> Empty
    ElectronHead -> ElectronTail
    ElectronTail -> Conductor
    Conductor
        | length (filter (== ElectronHead) friends) `elem` [1,2] ->
            ElectronHead
        | otherwise -> Conductor)
