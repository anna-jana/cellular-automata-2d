module Banks where

import CellularAutomata2D
import GUI

main :: IO ()
main = randomSpace (50, 50) [True, False] >>= runCellularAutomata2D banks >> return ()

banks :: Rule Bool
banks = Rule neumannIndexDeltas $ \self neighbors ->
    return $ case count neighbors of
        0 -> self
        1 -> self
        2 -> neighbors !! 1 == neighbors !! 2 && self
        _ -> True
