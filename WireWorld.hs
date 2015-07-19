import CellularAutomata2D

data Cell = Empty | Conductor | ElectronHead | ElectronTail
    deriving (Show, Eq, Enum, Bounded)

main :: IO ()
main = runCellularAutomata2D
    (initSpaceWithDefault Empty 50 50 [])
    [minBound..maxBound]
    (([grey, yellow, blue, red] !!) . fromEnum)
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
