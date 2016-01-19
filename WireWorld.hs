-- | A cellular automaton witch simulates logic circuits
module WireWorld where

import CellularAutomata2D
import GUI

-- | A wire can be empty (no wire), a conductor (no current), or an electron head or tail.
data Wire = Empty | Conductor | ElectronHead | ElectronTail deriving (Show, Eq, Enum, Bounded)

main :: IO ()
main = runCellularAutomata2D (initSpaceWithCells (50, 50) Empty [] :: Torus Wire) wireWorldRule

-- | A empty wire stays an empty wire forever.
--   An electron head becomes on electron tail (the electron moves)
--   An electron tail becomes a conductor (the electron moves)
--   A Conductor becomes an electron head if there one or two electron heads in his neighborhood.
--   Otherwise it stays a conductor.
--   Wire world uses a moor neighborhood.
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
