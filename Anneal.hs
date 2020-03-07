module Anneal where

import CellularAutomata2D
import GUI

main :: IO ()
main = randomSpace (50, 50) [True, False] >>= runCellularAutomata2D anneal >> return ()

anneal :: Rule Bool
anneal = makeVotingMoorRule $ \c -> c >= 6 || c == 4
