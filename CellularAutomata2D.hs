module CellularAutomata2D (
    Space, Rule,
    getCell, setCell, forSpace, getSpaceSize,
    randomSpace, initSpace, initSpaceWithDefault,
    update,
    makeRuleWithNeighbors,
    makeMoorRule, makeNeumanRule,
    makeTotalMoorRule,
    choice) where

import System.Random (randomRIO)
import Data.Array (listArray, bounds, indices, (!), Array, (//))
import Control.Monad (replicateM, forM_, guard)

type Rule a = Space a -> (Int, Int) -> IO a
type Space a = Array (Int, Int) a

getCell :: (Int, Int) -> Space a -> a
getCell = flip (!)

setCell :: Space a -> a -> (Int, Int) -> Space a
setCell space cell index = space // [(index, cell)]

forSpace :: Space a -> ((Int, Int) -> a -> IO ()) -> IO ()
forSpace space fn =
    forM_ [0..maxRow] $ \row ->
        forM_ [0..maxCol] $ \col ->
            fn (row, col) (space ! (row, col))
    where (_, (maxRow, maxCol)) = bounds space

getSpaceSize :: Space a -> (Int, Int)
getSpaceSize space = (maxRow + 1, maxCol + 1)
    where (_, (maxRow, maxCol)) = bounds space

------------------- creating spaces -----------------
randomSpace :: Int -> Int -> [a] -> IO (Space a)
randomSpace height width cellStateDist = fmap
    (listArray ((0, 0), (height-1, width-1)) . map (cellStateDist !!))
    (replicateM (width*height) (randomRIO (0, length cellStateDist - 1)))

initSpaceWithDefault :: a -> Int -> Int -> [((Int, Int), a)] -> Space a
initSpaceWithDefault defaultValue spaceWidth spaceHeight initCells = listArray
    ((0, 0), (spaceWidth - 1, spaceHeight - 1))
    (replicate (spaceWidth*spaceHeight) defaultValue) // initCells

initSpace :: Int -> Int -> [((Int, Int), Int)] -> Space Int
initSpace = initSpaceWithDefault (0 :: Int)

--------------------- updating and rules ----------------
update :: Space a -> Rule a -> IO (Space a)
update space updateCell = listArray (bounds space) `fmap`
    mapM (updateCell space) (indices space)

-- TODO: make topology not fixed to a torus
makeRuleWithNeighbors :: [(Int, Int)] -> (a -> [a] -> IO a) -> Rule a
makeRuleWithNeighbors neighborhoodDeltas ruleWithNeighbors
                      space (row, col) = do
    let (_, (maxRow, maxCol)) = bounds space
    let friends = map
            (\(drow, dcol) ->
                space ! ((row + drow) `mod` (maxRow + 1),
                         (col + dcol) `mod` (maxCol + 1)))
            neighborhoodDeltas
    let self = space ! (row, col)
    ruleWithNeighbors self friends

makeMoorRule, makeNeumanRule :: (a -> [a] -> IO a) -> Rule a
makeMoorRule = makeRuleWithNeighbors moorIndexDeltas
makeNeumanRule = makeRuleWithNeighbors neumannIndexDeltas

moorIndexDeltas :: [(Int, Int)]
moorIndexDeltas = do
    dx <- [-1..1]; dy <- [-1..1]
    guard $ not (dx == 0 && dy == 0)
    return (dy, dx)

neumannIndexDeltas :: [(Int, Int)]
neumannIndexDeltas = do
    dx <- [-1..1]; dy <- [-1..1]
    guard $ (dx == 0) /= (dy == 0)
    return (dy, dx)

makeTotalMoorRule :: [Int] -> [Int] -> Rule Int
makeTotalMoorRule stayAlive getBorn = makeMoorRule
    (\self friends -> return $ case self of
        0 -> if sum friends `elem` getBorn then 1 else 0
        1 -> if sum friends `elem` stayAlive then 1 else 0
        _ -> error $ "binary total rule: expected 0 or 1 but got " ++
            show self)

choice :: [a] -> IO a
choice xs = (xs !!) `fmap` randomRIO (0, length xs - 1)
