module RockPaperScissors where

import CellularAutomata2D
import GUI

main :: IO ()
main = do
    let space = initSpaceWithCells (50, 50) (RPSCell White maxLives) [] :: Torus RPSCell
    runCellularAutomata2D space (Rule moorIndexDeltas updateCell)

data CellColor = Red | Green | Blue | White deriving (Show, Eq, Bounded, Enum)
data RPSCell = RPSCell { color :: CellColor, lives :: Int } deriving (Show, Eq)

maxLives :: Int
maxLives = 4

updateCell :: RPSCell -> [RPSCell] -> IO RPSCell
updateCell self friends = fight self `fmap` choice friends

fight :: RPSCell -> RPSCell -> RPSCell
fight toUpdate other
    | color toUpdate == White = RPSCell (color other) maxLives
    | color other == White = toUpdate
    | color toUpdate == color other = toUpdate
    | better (color toUpdate) (color other) =
        toUpdate { lives = min (lives toUpdate + 1) maxLives }
    | otherwise = let updated = toUpdate { lives = lives toUpdate - 1 } in
        if lives updated == 0 then RPSCell (color other) maxLives else updated

better :: CellColor -> CellColor -> Bool
better Red Blue = True
better Green Red = True
better Blue Green = True
better _ _ = False

instance Cell RPSCell where
    getColor = getColorOfColor . color
    getSuccState c = c { color = cycleEnum $ color c }

getColorOfColor :: CellColor -> Color
getColorOfColor White = white
getColorOfColor Red   = red
getColorOfColor Blue  = blue
getColorOfColor Green = green
