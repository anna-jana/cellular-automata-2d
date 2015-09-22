import Control.Monad (replicateM)

import CellularAutomata2D
import GUI

main :: IO ()
main = mapM_ (\[isAnd, isMoor, withSelf] -> do
        putStr "isAnd: "
        print isAnd
        putStr "isMoor: "
        print isMoor
        putStr "withSelf: "
        print withSelf
        putStrLn ""
        logicSpace isAnd isMoor withSelf)
    (replicateM 3 [True, False])

logicSpace :: Bool -> Bool -> Bool -> IO ()
logicSpace isAnd isMoor withSelf = do
    space <- randomSpace 50 50 [True, False] :: IO (Torus Bool)
    -- let space = initSpaceWithDefault False 50 50 [] :: Torus Bool
    let ruleMaker = if isMoor then makeMoorRule else makeNeumanRule
    let logicFn = if isAnd then and else or
    let rule = ruleMaker (\self ns -> return $ not $ logicFn (if withSelf then self:ns else ns))
    runCellularAutomata2D space [True, False] colors rule

colors :: Bool -> Color
colors c
    | c = white
    | otherwise = black

