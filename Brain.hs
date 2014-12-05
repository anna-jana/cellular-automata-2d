module Brain where

import CellularAutomata2D

brain = do
    space <- randomSpace 50 50 [Ready, Firing, Refractory]
    runCellularAutomata2D space [Ready, Firing, Refractory]
        colors (makeMoorRule (\self -> return . rule self))

data Cell = Ready | Firing | Refractory deriving (Show, Eq, Enum)

colors Ready = 231241
colors Firing = 4546476
colors Refractory = 0

rule Ready friends
    | length (filter (== Firing) friends) == 2 = Firing
    | otherwise = Ready
rule Firing _ = Refractory
rule Refractory _ = Ready
