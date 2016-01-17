module WireWorld where

import CellularAutomata2D
import GUI

data Wire = Empty | Conductor | ElectronHead | ElectronTail deriving (Show, Eq, Enum, Bounded)

main :: IO ()
main = runCellularAutomata2D (initSpaceWithCells (50, 50) Empty [] :: Torus Wire) wireWorldRule

wireWorldRule :: Rule Wire
wireWorldRule = Rule moorIndexDeltas (\self friends -> return $ case self of
    Empty -> Empty
    ElectronHead -> ElectronTail
    ElectronTail -> Conductor
    Conductor
        | length (filter (== ElectronHead) friends) `elem` [1,2] ->
            ElectronHead
        | otherwise -> Conductor)

instance Cell Wire where
    getColor Empty = grey
    getColor Conductor = yellow
    getColor ElectronHead = blue
    getColor ElectronTail = red

    getSuccState = cycleEnum
