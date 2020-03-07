module Majority where

import CellularAutomata2D
import GUI

main :: IO ()
main = randomSpace (50, 50) [True, False] >>= runCellularAutomata2D majority >> return ()

majority :: Rule Bool
majority = makeVotingMoorRule $ \c -> c > 9 - c
