module CellularAutomata2D (
    Space, Rule,
    runCellularAutomata2D,
    randomSpace, initSpace, initSpaceWithDefault,
    makeRuleWithNeighbors,
    makeMoorRule, makeNeumanRule,
    makeTotalMoorRule,
    choice) where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Primitives as Draw
import Data.Array (listArray, bounds, indices, (!), Array, (//))
import System.Random (randomRIO)
import Control.Monad (replicateM, forM_, guard)
import Control.Concurrent (threadDelay)
import Data.Maybe (fromJust)

type Rule a = (Space a -> (Int, Int) -> IO a)
type Space a = Array (Int, Int) a

targetScreenWidth = 500

runCellularAutomata2D :: Eq a => Space a -> (a -> SDL.Pixel) -> Rule a -> IO ()
runCellularAutomata2D space colors updateCell = do
    let (_, (maxRow, maxCol)) = bounds space
    let spaceWidth = maxCol + 1
    let spaceHeight = maxRow + 1
    let cellSize = targetScreenWidth `div` spaceWidth
    let screenWidth = spaceWidth * cellSize
    let screenHeight = spaceHeight * cellSize
    SDL.init []
    screen <- SDL.setVideoMode screenWidth screenHeight 32 [SDL.DoubleBuf]
    loop screen space colors cellSize updateCell

loop :: Eq a => SDL.Surface -> Space a -> (a -> SDL.Pixel) -> Int -> Rule a -> IO ()
loop screen space colors cellSize updateCell = do
    quit <- isQuit
    if quit
        then SDL.quit
        else do
            draw screen space colors cellSize
            threadDelay $ 10^5 * 2
            newSpace <- update space updateCell
            loop screen newSpace colors cellSize updateCell
    where isQuit = SDL.pollEvent >>= \e -> case e of
            SDL.NoEvent -> return False
            SDL.Quit -> return True
            _ -> isQuit

draw :: Eq a => SDL.Surface -> Space a -> (a -> SDL.Pixel) -> Int -> IO ()
draw screen space colors cellSize = do
    SDL.fillRect screen Nothing (SDL.Pixel 0)
    -- let pixelFormat = SDL.surfaceGetPixelFormat screen
    let (_, (maxRow, maxCol)) = bounds space
    forM_ [0..maxRow] $ \row ->
        forM_ [0..maxCol] $ \col -> do
            let color = colors (space ! (row, col))
            let top = cellSize*row
            let left = cellSize*col
            Draw.box screen (SDL.Rect left top (left + cellSize) (top + cellSize)) color
    SDL.flip screen
    where unsafeLookup key assoc = fromJust $ lookup key assoc

randomSpace :: Int -> Int -> [a] -> IO (Space a)
randomSpace height width cellStateDist = fmap
    (listArray ((0, 0), (height-1, width-1)) . map (cellStateDist !!))
    (replicateM (width*height) (randomRIO (0, length cellStateDist - 1)))

initSpaceWithDefault :: a -> Int -> Int -> [((Int, Int), a)] -> Space a
initSpaceWithDefault defaultValue spaceWidth spaceHeight initCells = listArray
    ((0, 0), (spaceWidth - 1, spaceHeight - 1))
    (replicate (spaceWidth*spaceHeight) defaultValue) // initCells

initSpace = initSpaceWithDefault 0

update :: Space a -> Rule a -> IO (Space a)
update space updateCell = listArray (bounds space) `fmap` mapM (updateCell space) (indices space)

makeRuleWithNeighbors :: [(Int, Int)] -> (a -> [a] -> IO a) -> Rule a
makeRuleWithNeighbors neighborhoodDeltas ruleWithNeighbors = \space (row, col) -> do
    let (_, (maxRow, maxCol)) = bounds space
    let friends = map
            (\(drow, dcol) ->
                space ! ((row + drow) `mod` (maxRow + 1), (col + dcol) `mod` (maxCol + 1)))
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
        _ -> error $ "binary total rule: expected 0 or 1 but got " ++ show self)

choice xs = (xs !!) `fmap` randomRIO (0, length xs - 1)
