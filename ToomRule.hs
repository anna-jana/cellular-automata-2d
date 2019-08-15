module ToomsRule where

import CellularAutomata2D
import GUI

data Spin = Up | Down deriving (Show, Eq, Bounded, Enum)

spinToInt :: Spin -> Int
spinToInt Up = 1
spinToInt Down = -1

instance Cell Spin where
    getColor Up = red
    getColor Down = green
    getSuccState = cycleEnum

detToomsRule :: Spin -> [Spin] -> Spin
detToomsRule self neighbors = if sum (map spinToInt $ self : neighbors) > 0 then Up else Down -- the sum can't be 0

randomize :: Spin -> IO Spin
randomize Up = chooseWithPropability p Down Up
randomize Down = chooseWithPropability q Up Down

p, q :: Double
p = 0.1
q = 0.1

toomsRule :: Rule Spin
toomsRule = Rule [(-1, 0), (0, 1)] $ \self neighbors -> randomize $ detToomsRule self neighbors

main = randomSpace (50, 50) [Up, Down] >>= runCellularAutomata2D toomsRule >> return ()



