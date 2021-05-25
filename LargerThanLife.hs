module LargerThanLife where

import CellularAutomata2D
import GUI

-- | Run a simple glider simulation.
main :: IO ()
main = randomSpace (50, 50) [False, True] >>= runCellularAutomata2D bugsRule >> return ()

-- | http://www.mirekw.com/ca/rullex_lgtl.html
bugsRule :: Rule Bool
bugsRule = Rule (moorIndexDeltasRadiusN 5) (\self neighbors ->
    let neighborhoodSum = sum $ map (\b -> if b then 1 else 0) neighbors in
    return $ (self && 34 <= neighborhoodSum && neighborhoodSum <= 58) || (not self && 34 <= neighborhoodSum && neighborhoodSum <= 45))
