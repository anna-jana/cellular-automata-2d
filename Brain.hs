-- | a cellular automata witch is inspired by neurons in our brain
module Brain (Neuron(..), brainRule, main) where

import CellularAutomata2D
import GUI

-- | simulate Brians Brain
main :: IO ()
main = randomSpace (50, 50) [Ready, Firing, Refractory] >>= runCellularAutomata2D brainRule

-- | the state of a neuron
data Neuron = Ready | Firing | Refractory deriving (Show, Eq, Bounded, Enum)

instance Cell Neuron where
    getColor Ready = white
    getColor Firing = black
    getColor Refractory = grey

    getSuccState = cycleEnum

-- | A firing cell becomes refactoring and a refactoring cell becomes ready.
--   The Cell stays ready until two neighbors of the cell are firing. Now cell is firing too.
--   Brian's Brain uses a moor neighborhood.
brainRule :: Rule Neuron
brainRule = Rule moorIndexDeltas (\self -> return . rule self)
    where
        rule Ready friends
            | length (filter (== Firing) friends) == 2 = Firing
            | otherwise = Ready
        rule Firing _ = Refractory
        rule Refractory _ = Ready
