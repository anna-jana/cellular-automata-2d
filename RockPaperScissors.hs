-- | A cellular automaton witch simulates a lot of rock paper scissor games or
--   something witch is similar to oscillating chemical reactions.
module RockPaperScissors where

import CellularAutomata2D
import GUI

-- Simulate the rock paper scissors automaton
main :: IO ()
main = do
    let space = initSpaceWithCells (50, 50) (RPSCell White maxLives) [] :: Torus RPSCell
    runCellularAutomata2D space rockPaperScissorsRule

-- Every cell can have a color of be white (empty)
data CellColor = Red | Green | Blue | White deriving (Show, Eq, Bounded, Enum)
-- A cell has also a number of lives.
data RPSCell = RPSCell { color :: CellColor, lives :: Int } deriving (Show, Eq)

-- The initial number of lives.
maxLives :: Int
maxLives = 4

-- | In each step a cell selects one neighbor cell at random.
--   Then here fight against each other. If the our cell loses it loses one life, if it wins it gains one life point.
--   If our cell doesn't have any lives left it dies and gets the color of the enemy as well as `maxLives` lives.
--   An empty cell as an enemy is always ignored (we don't change our state).
--   An empty cell itself also selects one neighbor a random and then becomes this neighbor with `maxLives` lives.
--   The rock paper scissors automaton uses a moor neighborhood.
rockPaperScissorsRule :: Rule RPSCell
rockPaperScissorsRule = Rule moorIndexDeltas (\self friends -> fight self `fmap` choice friends)

fight :: RPSCell -> RPSCell -> RPSCell
fight toUpdate other
    | color toUpdate == White = RPSCell (color other) maxLives
    | color other == White = toUpdate
    | color toUpdate == color other = toUpdate
    | better (color toUpdate) (color other) =
        toUpdate { lives = min (lives toUpdate + 1) maxLives }
    | otherwise = let updated = toUpdate { lives = lives toUpdate - 1 } in
        if lives updated == 0 then RPSCell (color other) maxLives else updated

-- | blue > green > red > blue
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
