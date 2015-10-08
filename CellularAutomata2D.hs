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

-- | A Rule is a function that returns a new cell value for a given cell coordinate in a given space.
type Rule s a = s a -> (Int, Int) -> IO a

class Space s where
    -- | Get a cell at a coordinate in the space.
    getCell :: (Int, Int) -> s a -> a
    -- | Set a cell at a coordinate in the space to a new value.
    setCell :: s a -> a -> (Int, Int) -> s a
    -- | Get the dimensions of the space.
    getSpaceSize :: s a -> (Int, Int)
    -- | Initializes the space using a given function that takes a coordinate
    -- | and returns the cell value in the IO Monad (so you can easily e.g. generate
    -- | a random space configuration).
    initSpaceIO :: (Int, Int) -> ((Int, Int) -> IO a) -> IO (s a)
    -- | Initializes the space using a pure function witch take the coordinate of
    -- | the cell and returns the cell value
    initSpace :: (Int, Int) -> ((Int, Int) -> a) -> s a

    -- | Set a bunch of cells specified by there coordinate to new values.
    -- | This function has a default implementation in terms of setCell
    -- | but it might be specialized for performence purposes.
    setCells :: s a -> [((Int, Int), a)] -> s a
    setCells = foldl (\s c -> setCell s (snd c) (fst c))

    -- | Updates a given space by one generation using a given rule.
    update :: s a -> Rule s a -> IO (s a)
    update space updateCell = initSpaceIO (getSpaceSize space) (updateCell space)

----------------------- torus array space -----------------------
-- | A Torus is basicly a plane with top and botton connected as well as left and right connected.
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
-- | Iterates over a given space and calls a given function on the
-- | coordinate and value of each cells.
-- | This is done in row mayor order.
forSpace :: Space s => s a -> ((Int, Int) -> a -> IO ()) -> IO ()
forSpace space fn =
    forM_ [0..spaceHeight - 1] $ \row ->
        forM_ [0..spaceWidth - 1] $ \col ->
            fn (row, col) (getCell (row, col) space)
    where (spaceHeight, spaceWidth) = getSpaceSize space

------------------- creating spaces -----------------
-- | Initializes a space of a given shape using a list of possible cells.
-- | Each cell is randomly choosen from the list.
-- | You might want to duplicate elements in the list to ajust the frequencys
-- | (probability to be choosen) of the cell values.
randomSpace :: Space s => Int -> Int -> [a] -> IO (s a)
randomSpace height width cellStateDist = initSpaceIO (height, width) $ \_ ->
    (cellStateDist !!) <$> randomRIO (0, length cellStateDist - 1)

-- | Initializes a space with a default background cell value and a few cells
-- | at given coordinates with individual values.
initSpaceWithDefault :: Space s => a -> Int -> Int -> [((Int, Int), a)] -> s a
initSpaceWithDefault defaultValue spaceWidth spaceHeight initCells =
    setCells (initSpace (spaceHeight, spaceWidth) (const defaultValue)) initCells

-- | Specialized version of initSpaceWithDefault for int spaces with 0 as the background.
initSpaceWithCells :: Space s => Int -> Int -> [((Int, Int), Int)] -> s Int
initSpaceWithCells = initSpaceWithDefault (0 :: Int)

--------------------- updating and rules ----------------
-- | Creates a rule from a function witch takes the cell value and a list of neightbors and
-- | returns the new cell value in the IO Monad so that you can implement nondeterminstic
-- | (stochastic) automata. The neighborhood is specified by a list of deltas from the cell
-- | coordinate.
makeRuleWithNeighbors :: Space s => [(Int, Int)] -> (a -> [a] -> IO a) -> Rule s a
makeRuleWithNeighbors neighborhoodDeltas ruleWithNeighbors
                      space (row, col) = ruleWithNeighbors self friends
    where self = getCell (row, col) space
          friends = map (\(dr, dc) -> getCell (row + dr, col + dc) space) neighborhoodDeltas

makeMoorRule, makeNeumanRule :: Space s => (a -> [a] -> IO a) -> Rule s a
-- | Specialized version of makeRuleWithNeighbors for the Moor neightborhood.
-- | This is the standard neighborhood for a lot of automata like conways game of life
makeMoorRule = makeRuleWithNeighbors moorIndexDeltas
-- | Specialized version of makeRuleWithNeighbors for the von Neuman neightborhood.
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

-- | Creates a life like automata from a list of neightborhood sizes in witch
-- | a new cell is born and a list of neightborhood sizes where the cell stays
-- | alive. e.g. the game of life is makeTotalMoorRule [2,3] [3]
makeTotalMoorRule :: Space s => [Int] -> [Int] -> Rule s Int
makeTotalMoorRule stayAlive getBorn = makeMoorRule
    (\self friends -> return $ case self of
        0 -> if sum friends `elem` getBorn then 1 else 0
        1 -> if sum friends `elem` stayAlive then 1 else 0
        _ -> error $ "binary total rule: expected 0 or 1 but got " ++
            show self)

-- | Selectes one random element from a list.
-- | The list has to be finit.
-- | O(n)
choice :: [a] -> IO a
choice xs = (xs !!) `fmap` randomRIO (0, length xs - 1)
