-- | The forest automata is a nondeterministic cellular automata witch simulates a forest fire
module Forest where

import CellularAutomata2D
import GUI
import System.Random (randomRIO)

data Wood = Tree | Empty | Fire deriving (Show, Eq, Bounded, Enum)

-- | run the simulation
main :: IO ()
main = randomSpace (150, 150) [Empty, Tree] >>= runCellularAutomata2D forestRule

-- | Probability of a fire in a tree if one the neighbor tree is burning
newFireProb :: Float
newFireProb = 1 - 0.999

-- | Probability of a new tree to grow
newTreeProb :: Float
newTreeProb = 1 - 0.96

-- | If a tree is burning, it becomes plain empty ground
--   A new tree grows one empty ground with probability `newTreeProb`.
--   If a tree as at least one burning neighbor tree it starts to burn itself witch probability `newFireProb`.
--   Otherwise everything stays the same.
--   The forest automaton uses the moor neighborhood.
forestRule :: Rule Wood
forestRule = Rule moorIndexDeltas updateCell
    where
        updateCell Fire _ = return Empty
        updateCell Tree friends = do
            newFire <- randomRIO (0,1)
            return $ if newFireProb >= newFire || Fire `elem` friends
                then Fire
                else Tree
        updateCell Empty _ = do
            newTree <- randomRIO (0, 1)
            return $ if newTreeProb >= newTree
                then Tree
                else Empty

instance Cell Wood where
    getColor Fire = red
    getColor Tree = green
    getColor Empty = brown

    getSuccState = cycleEnum
