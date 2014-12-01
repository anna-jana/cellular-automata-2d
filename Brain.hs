module Brain where

import CellularAutomata2D
import qualified Graphics.UI.SDL as SDL

brain = do
    space <- randomSpace 50 50 [Ready, Firing, Refractory]
    runCellularAutomata2D space [Ready, Firing, Refractory]
        colors (makeMoorRule (\self -> return . rule self))

data Cell = Ready | Firing | Refractory deriving (Show, Eq, Enum)

colors Ready = SDL.Pixel 231241
colors Firing = SDL.Pixel 4546476
colors Refractory = SDL.Pixel 0

rule Ready friends
    | length (filter (== Firing) friends) == 2 = Firing
    | otherwise = Ready
rule Firing _ = Refractory
rule Refractory _ = Ready
