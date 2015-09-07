import CellularAutomata2D
import GUI

main :: IO ()
main = do
    space <- randomSpace 50 50 [Ready, Firing, Refractory] :: IO (Torus Cell)
    runCellularAutomata2D space [Ready, Firing, Refractory]
        colors (makeMoorRule (\self -> return . rule self))

data Cell = Ready | Firing | Refractory deriving (Show, Eq)

colors :: Cell -> Color
colors Ready = white
colors Firing = black
colors Refractory = grey

rule :: Cell -> [Cell] -> Cell
rule Ready friends
    | length (filter (== Firing) friends) == 2 = Firing
    | otherwise = Ready
rule Firing _ = Refractory
rule Refractory _ = Ready
