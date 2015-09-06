import CellularAutomata2D
import GUI
import System.Random (randomRIO)

data Wood = Tree | Empty | Fire deriving (Show, Eq)

main :: IO ()
main = do
    space <- randomSpace 150 150 [Empty, Tree]
    runCellularAutomata2D space [Tree, Empty, Fire] colors
        (makeMoorRule updateCell)

newFireProp, newTreeProp :: Float
newFireProp = 1 - 0.999
newTreeProp = 1 - 0.96

updateCell :: Wood -> [Wood] -> IO Wood
updateCell Fire _ = return Empty
updateCell Tree friends = do
    newFire <- randomRIO (0,1)
    return $ if newFireProp >= newFire || Fire `elem` friends
        then Fire
        else Tree
updateCell Empty _ = do
    newTree <- randomRIO (0, 1)
    return $ if newTreeProp >= newTree
        then Tree
        else Empty

colors :: Wood -> Color
colors Fire = red
colors Tree = green
colors Empty = brown
