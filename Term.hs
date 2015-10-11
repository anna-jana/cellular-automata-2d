module Term (runInTerminal) where

import Control.Monad (when)

import CellularAutomata2D

-- | Runs a 2d cellular automata in the terminal using
-- a function witch returns a char for a cell state,
-- an initial space and a rule.
-- The user can press enter to view the next generation.
runInTerminal :: Space s => (a -> Char) -> s a -> Rule a -> IO ()
runInTerminal char space rule = do
    printSpace char space
    getLine
    space' <- update space rule
    runInTerminal char space' rule

printSpace :: Space s => (a -> Char) -> s a -> IO ()
printSpace char space = forSpace space $ \(_, col) cell -> do
        putChar (char cell)
        when (col == spaceWidth - 1) $ putChar '\n'
    where (_, spaceWidth) = getSpaceSize space

