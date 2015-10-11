module CellularAutomata2D (
    Rule,
    Space(..),
    Torus(..),
    forSpace,
    randomSpace, initSpaceWithCells, initIntSpaceWithCells,
    makeMoorRule, makeNeumanRule,
    makeTotalMoorRule,
    choice) where

import System.Random (randomRIO, Random)
import Data.Array (listArray, bounds, (!), Array, (//))
import Control.Monad (forM_, guard)
import Control.Applicative ((<$>))

-- | A Rule is a function that returns a new cell value for an old state.
-- The new cell value is in the IO Monad so that you can implement nondeterminstic
-- (stochastic) automata. The neighborhood is specified by a list of offsets from the cell
-- coordinate.
data Rule a = Rule
    { ruleNeighborhoodDeltas :: [(Int, Int)]
    , ruleFunction :: a -> [a] -> IO a
    }

class Space s where
    -- | Get a cell at a coordinate in the space.
    getCell :: s a -> (Int, Int) -> a
    -- | Set a cell at a coordinate in the space to a new value.
    setCell :: s a -> (Int, Int) -> a -> s a
    -- | Get the dimensions of the space.
    getSpaceSize :: s a -> (Int, Int)
    -- | Initializes the space using a given function that takes a coordinate
    -- and returns the cell value in the IO Monad (so you can easily e.g. generate
    -- a random space configuration).
    initSpaceIO :: (Int, Int) -> ((Int, Int) -> IO a) -> IO (s a)
    -- | Initializes the space using a pure function witch take the coordinate of
    -- the cell and returns the cell value
    initSpace :: (Int, Int) -> ((Int, Int) -> a) -> s a

    -- | Set a bunch of cells specified by there coordinate to new values.
    -- This function has a default implementation in terms of setCell
    -- but it might be specialized for performence purposes.
    setCells :: s a -> [((Int, Int), a)] -> s a
    setCells = foldl (\s (i, v) -> setCell s i v)

    -- | Updates a given space by one generation using a given rule.
    update :: s a -> Rule a -> IO (s a)
    update space rule = initSpaceIO (getSpaceSize space) updateCell
      where
          updateCell (row, col) = ruleFunction rule self friends
            where
              friends = map (\(dr, dc) -> getCell space (row + dr, col + dc)) (ruleNeighborhoodDeltas rule)
              self = getCell space (row, col)

----------------------- torus array space -----------------------
-- | A Torus is basicly a plane with top and botton connected as well as left and right connected.
newtype Torus a = Torus (Array (Int, Int) a) deriving (Show, Eq)

instance Space Torus where
    getCell (Torus a) (row, col) = a ! (row `mod` h, col `mod` w)
        where (h, w) = getSpaceSize (Torus a)
    setCell (Torus space) index cell = Torus $ space // [(index, cell)]
    setCells (Torus space) cells = Torus $ space // cells
    getSpaceSize (Torus space) = (maxRow + 1, maxCol + 1)
        where (_, (maxRow, maxCol)) = bounds space
    initSpaceIO (h, w) initFn = Torus . listArray ((0, 0), (h - 1, w - 1)) <$>
        sequence [initFn (row, col) | row <- [0..h-1], col <- [0..w-1]]
    initSpace (h, w) initFn = Torus $ listArray ((0, 0), (h - 1, w - 1))
        [initFn (row, col) | row <- [0..h-1], col <- [0..w-1]]

------------------ space interation ------------------
-- | Iterates over a given space and calls a given function on the
-- coordinate and value of each cells.
-- This is done in row mayor order.
forSpace :: Space s => s a -> ((Int, Int) -> a -> IO ()) -> IO ()
forSpace space fn =
    forM_ [0..spaceHeight - 1] $ \row ->
        forM_ [0..spaceWidth - 1] $ \col ->
            fn (row, col) (getCell space (row, col))
    where (spaceHeight, spaceWidth) = getSpaceSize space

------------------- creating spaces -----------------
-- | Initializes a space of a given shape using a list of possible cells.
-- Each cell is randomly choosen from the list.
-- You might want to duplicate elements in the list to ajust the frequencys
-- (probability to be choosen) of the cell values.
randomSpace :: Space s => (Int, Int) -> [a] -> IO (s a)
randomSpace (height, width) cellStateDist = initSpaceIO (height, width) $ const $ choice cellStateDist

-- | Initializes a space with a default background cell value and a few cells
-- at given coordinates with individual values.
initSpaceWithCells :: Space s => (Int, Int) -> a -> [((Int, Int), a)] -> s a
initSpaceWithCells (spaceWidth, spaceHeight) defaultValue =
    setCells (initSpace (spaceHeight, spaceWidth) (const defaultValue))

-- | Specialized version of initSpaceWithDefault for int spaces with 0 as the background.
initIntSpaceWithCells :: Space s => (Int, Int) -> [((Int, Int), Int)] -> s Int
initIntSpaceWithCells = flip initSpaceWithCells (0 :: Int)

--------------------- updating and rules ----------------

makeMoorRule, makeNeumanRule :: (a -> [a] -> IO a) -> Rule a
-- ^ Specialized version of makeRuleWithNeighbors for the Moor neightborhood.
-- This is the standard neighborhood for a lot of automata like conways game of life
makeMoorRule = Rule moorIndexDeltas
-- ^ Specialized version of makeRuleWithNeighbors for the von Neuman neightborhood.
makeNeumanRule = Rule neumannIndexDeltas

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
-- a new cell is born and a list of neightborhood sizes where the cell stays
-- alive. e.g. the game of life is makeTotalMoorRule [2,3] [3]
makeTotalMoorRule :: [Int] -> [Int] -> Rule Int
makeTotalMoorRule stayAlive getBorn = makeMoorRule
    (\self friends -> return $ case self of
        0 -> if sum friends `elem` getBorn then 1 else 0
        1 -> if sum friends `elem` stayAlive then 1 else 0
        _ -> error $ "binary total rule: expected 0 or 1 but got " ++
            show self)

-- | Selectes one random element from a list.
-- The list has to be finit.
-- O(n)
choice :: [a] -> IO a
choice xs = (xs !!) `fmap` randomRIO (0, length xs - 1)
