module Term (runInTerminal) where

import Control.Monad (when)

import CellularAutomata2D

runInTerminal :: (a -> Char) -> Space a -> Rule a -> IO ()
runInTerminal char space rule = do
    printSpace char space
    getLine
    space' <- update space rule
    runInTerminal char space' rule

printSpace :: (a -> Char) -> Space a -> IO ()
printSpace char space = forSpace space $ \(_, col) cell -> do
        putChar (char cell)
        when (col == spaceWidth - 1) $ putChar '\n'
    where (_, spaceWidth) = getSpaceSize space

