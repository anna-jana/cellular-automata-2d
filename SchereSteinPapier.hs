module SchereSteinPapier where

import CellularAutomata2D

schereSteinPapier :: Space Cell -> IO ()
schereSteinPapier space = runCellularAutomata2D space
    (map (flip Cell maxLives) [Red, Green, Blue, White])
    colors (makeMoorRule updateCell)

data CellColor = Red | Green | Blue | White deriving (Show, Eq)
data Cell = Cell { color :: CellColor, lives :: Int } deriving (Show, Eq)

maxLives :: Int
maxLives = 4

updateCell :: Cell -> [Cell] -> IO Cell
updateCell self friends = do
    enemy <- choice friends
    return $ fight self enemy

fight :: Cell -> Cell -> Cell
fight toUpdate other
    | color toUpdate == White = Cell (color other) maxLives
    | color other == White = toUpdate
    | color toUpdate == color other = toUpdate
    | better (color toUpdate) (color other) = toUpdate { lives = min (lives toUpdate + 1) maxLives }
    | otherwise = let updated = toUpdate { lives = lives toUpdate - 1 } in
        if lives updated == 0 then Cell (color other) maxLives else updated

better :: CellColor -> CellColor -> Bool
better Red Blue = True
better Green Red = True
better Blue Green = True
better _ _ = False

colors :: Cell -> Color
colors = getColor . color

getColor :: CellColor -> Color
getColor White = white
getColor Red   = red
getColor Blue  = blue
getColor Green = green

simpleSpace :: Space Cell
simpleSpace = initSpaceWithDefault (Cell White maxLives) 50 50
    [((10, 10), Cell Red maxLives), ((10,11), Cell Blue maxLives), ((11,10), Cell Green maxLives)]
