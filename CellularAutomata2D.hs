module CellularAutomata2D (
    Rule,
    Space(..),
    Torus(..),
    forSpace,
    randomSpace, initSpaceWithCells, initSpaceWithDefault,
    makeRuleWithNeighbors,
    makeMoorRule, makeNeumanRule,
    makeTotalMoorRule,
    choice) where

import System.Random (randomRIO, Random)
import Data.Array (listArray, bounds, indices, (!), Array, (//))
import Control.Monad (replicateM, forM_, guard)
import Control.Applicative ((<$>))

type Rule s a = s a -> (Int, Int) -> IO a

class Space s where
    getCell :: (Int, Int) -> s a -> a
    setCell :: s a -> a -> (Int, Int) -> s a
    getSpaceSize :: s a -> (Int, Int)
    initSpaceIO :: (Int, Int) -> ((Int, Int) -> IO a) -> IO (s a)
    initSpace :: (Int, Int) -> ((Int, Int) -> a) -> s a

    setCells :: s a -> [((Int, Int), a)] -> s a
    setCells = foldl (\s c -> setCell s (snd c) (fst c))

    update :: s a -> Rule s a -> IO (s a)
    update space updateCell = initSpaceIO (getSpaceSize space) (updateCell space)

----------------------- torus array space -----------------------
newtype Torus a = Torus (Array (Int, Int) a) deriving (Show, Eq)

instance Space Torus where
    getCell (row, col) (Torus a) = a ! (row `mod` h, col `mod` w)
        where (h, w) = getSpaceSize (Torus a)
    setCell (Torus space) cell index = Torus $ space // [(index, cell)]
    setCells (Torus space) cells = Torus $ space // cells
    getSpaceSize (Torus space) = (maxRow + 1, maxCol + 1)
        where (_, (maxRow, maxCol)) = bounds space
    initSpaceIO (h, w) initFn = Torus . listArray ((0, 0), (h - 1, w - 1)) <$>
        sequence [initFn (row, col) | row <- [0..h-1], col <- [0..w-1]]
    initSpace (h, w) initFn = Torus $ listArray ((0, 0), (h - 1, w - 1))
        [initFn (row, col) | row <- [0..h-1], col <- [0..w-1]]

------------------ space interation ------------------
forSpace :: Space s => s a -> ((Int, Int) -> a -> IO ()) -> IO ()
forSpace space fn =
    forM_ [0..spaceHeight - 1] $ \row ->
        forM_ [0..spaceWidth - 1] $ \col ->
            fn (row, col) (getCell (row, col) space)
    where (spaceHeight, spaceWidth) = getSpaceSize space

------------------- creating spaces -----------------
randomSpace :: Space s => Int -> Int -> [a] -> IO (s a)
randomSpace height width cellStateDist = initSpaceIO (height, width) $ \_ ->
    (cellStateDist !!) <$> randomRIO (0, length cellStateDist - 1)

initSpaceWithDefault :: Space s => a -> Int -> Int -> [((Int, Int), a)] -> s a
initSpaceWithDefault defaultValue spaceWidth spaceHeight initCells =
    setCells (initSpace (spaceHeight, spaceWidth) (const defaultValue)) initCells

initSpaceWithCells :: Space s => Int -> Int -> [((Int, Int), Int)] -> s Int
initSpaceWithCells = initSpaceWithDefault (0 :: Int)

--------------------- updating and rules ----------------
makeRuleWithNeighbors :: Space s => [(Int, Int)] -> (a -> [a] -> IO a) -> Rule s a
makeRuleWithNeighbors neighborhoodDeltas ruleWithNeighbors
                      space (row, col) = ruleWithNeighbors self friends
    where self = getCell (row, col) space
          friends = map (\(dr, dc) -> getCell (row + dr, col + dc) space) neighborhoodDeltas

makeMoorRule, makeNeumanRule :: Space s => (a -> [a] -> IO a) -> Rule s a
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

makeTotalMoorRule :: Space s => [Int] -> [Int] -> Rule s Int
makeTotalMoorRule stayAlive getBorn = makeMoorRule
    (\self friends -> return $ case self of
        0 -> if sum friends `elem` getBorn then 1 else 0
        1 -> if sum friends `elem` stayAlive then 1 else 0
        _ -> error $ "binary total rule: expected 0 or 1 but got " ++
            show self)

choice :: [a] -> IO a
choice xs = (xs !!) `fmap` randomRIO (0, length xs - 1)
