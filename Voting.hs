{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Voting where

import CellularAutomata2D
import GUI

main :: IO ()
main = do
    -- space <- randomSpace size [True, False]
    -- runCellularAutomata2D space [True, False] (\c -> if c then white else black) anneal
    space <- randomSpace size [0,1]
    runCellularAutomata2D space  banks

banks :: Rule Int
banks = Rule neumannIndexDeltas $ \self neighbors ->
    return $ case sum neighbors of
        0 -> self
        1 -> self
        2 -> if neighbors !! 1 == neighbors !! 2 then self else 0
        _ -> 1

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

instance Cell Int where
    getColor = ([white, black] !!)
    getSuccState 0 = 1
    getSuccState 1 = 0
