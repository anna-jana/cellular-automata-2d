import CellularAutomata2D

data Cell = Empty | ElectronHead | ElectronTail | Conductor
    deriving (Show, Eq, Enum)

main :: IO ()
main = runCellularAutomata2D
    (initSpaceWithDefault Empty 50 50 [])
    [Empty .. Conductor]
    (([grey, blue, red, yellow] !!) . fromEnum)
    update

update :: Rule Cell
update = makeMoorRule (\self friends -> return $ case self of
    Empty -> Empty
    ElectronHead -> ElectronTail
    ElectronTail -> Conductor
    Conductor
        | length (filter (== ElectronHead) friends) `elem` [1,2] ->
            ElectronHead
        | otherwise -> Conductor)
