import CellularAutomata2D
import GUI

data Cell = Empty | Conductor | ElectronHead | ElectronTail
    deriving (Show, Eq, Enum, Bounded)

main :: IO ()
main = runCellularAutomata2D
    (initSpaceWithDefault Empty 50 50 [] :: Torus Cell)
    [minBound..maxBound]
    (([grey, yellow, blue, red] !!) . fromEnum)
    wireWorldRule

wireWorldRule :: Space s => Rule s Cell
wireWorldRule = makeMoorRule (\self friends -> return $ case self of
    Empty -> Empty
    ElectronHead -> ElectronTail
    ElectronTail -> Conductor
    Conductor
        | length (filter (== ElectronHead) friends) `elem` [1,2] ->
            ElectronHead
        | otherwise -> Conductor)
