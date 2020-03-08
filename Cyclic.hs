module Cyclic where

import CellularAutomata2D
import GUI

data Mod = Mod Int Int deriving (Show, Eq)


instance Cell Mod where
    getColor (Mod a b) = colors !! mod a b
    getSuccState (Mod a b) = Mod ((a + 1) `mod` b) b

makeCyclicRule :: Int -> Int -> Rule Mod
makeCyclicRule numStates threshold = Rule moorIndexDeltas $ \self neighbors -> do
    let next = getSuccState self
    return $ if countCells next neighbors >= threshold then next else self

runCyclic :: Int -> Int -> (Int, Int) -> IO ()
runCyclic numStates threshold size = do
    initialState <- randomSpace size (map (`Mod` numStates) [0..numStates - 1])
    let rule = makeCyclicRule numStates threshold
    runCellularAutomata2D rule initialState >> return ()

main :: IO ()
main = runCyclic 8 2 (50, 50)
