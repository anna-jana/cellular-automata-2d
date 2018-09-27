{-# OPTIONS_GHC -fno-warn-type-defaults #-}
-- | Voting rules from *Cellular Automata Machines*
module Voting (main, banks, anneal, majority) where

import CellularAutomata2D
import GUI

main :: IO ()
main =
    -- randomSpace size [True, False] >>= runCellularAutomata2D anneal
    randomSpace size [True, False] >>= runCellularAutomata2D banks
    >> return ()

banks :: Rule Bool
banks = Rule neumannIndexDeltas $ \self neighbors ->
    return $ case count neighbors of
        0 -> self
        1 -> self
        2 -> neighbors !! 1 == neighbors !! 2 && self
        _ -> True

anneal :: Rule Bool
anneal = Rule moorIndexDeltas $ \self neighbors ->
    let count = length $ filter id $ self : neighbors
    in return $ count >= 6 || count == 4

majority :: Rule Bool
majority = Rule moorIndexDeltas $ \self neighbors -> return $
    length (filter id $ self : neighbors) > length (filter not $ self : neighbors)

circle :: Torus Bool
circle = initSpace size (\(row, col) -> 20 >=
    sqrt (fromIntegral ((row - div (fst size) 2)^2 + (col - div (snd size) 2)^2)))

size :: (Int, Int)
size = (50, 50)
