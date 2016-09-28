-- | procedural level generation (from https://jeremykun.com/2012/07/29/the-cellular-automaton-method-for-cave-generation/)
module Cave where

import CellularAutomata2D
import GUI

main :: IO ()
main = randomSpace (100, 100) [True, False] >>= runCellularAutomata2D caveRule

caveRule :: Rule Bool
caveRule = makeTotalMoorRule [6..8] [3..8]

smoothingRule :: Rule Bool
smoothingRule = makeTotalMoorRule [5..8] [5..8]
