module Brain where

import CellularAutomata2D
import GUI

main :: IO ()
main = do
    space <- randomSpace (50, 50) [Ready, Firing, Refractory] :: IO (Torus Neuron)
    runCellularAutomata2D space (Rule moorIndexDeltas (\self -> return . rule self))

data Neuron = Ready | Firing | Refractory deriving (Show, Eq, Bounded, Enum)

instance Cell Neuron where
    getColor Ready = white
    getColor Firing = black
    getColor Refractory = grey

    getSuccState = cycleEnum

rule :: Neuron -> [Neuron] -> Neuron
rule Ready friends
    | length (filter (== Firing) friends) == 2 = Firing
    | otherwise = Ready
rule Firing _ = Refractory
rule Refractory _ = Ready
