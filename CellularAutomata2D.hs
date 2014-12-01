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
import Data.Word (Word32)

type Rule a = (Space a -> (Int, Int) -> IO a)
type Space a = Array (Int, Int) a

targetScreenWidth = 500

runCellularAutomata2D :: Eq a => Space a -> [a] -> (a -> Word32) -> Rule a -> IO ()
runCellularAutomata2D space states colors updateCell = do
    let (_, (maxRow, maxCol)) = bounds space
    let spaceWidth = maxCol + 1
    let spaceHeight = maxRow + 1
    let cellSize = targetScreenWidth `div` spaceWidth
    let screenWidth = spaceWidth * cellSize
    let screenHeight = spaceHeight * cellSize
    SDL.init []
    screen <- SDL.setVideoMode screenWidth screenHeight 32 [SDL.DoubleBuf]
    loop $ SimulationState screen (SDL.Pixel . colors) cellSize updateCell space 0 False states

data PrivateEvent
    = No
    | Quit
    | Insert Int Int
    | NextColor
    | StartStop
    | EmptySpace
    | RandomSpace
    deriving (Show, Eq)

data SimulationState a = SimulationState
    { screen :: SDL.Surface
    , colors :: a -> SDL.Pixel
    , cellSize :: Int
    , updateCell :: Rule a
    , space :: Space a
    , accColor :: Int
    , running :: Bool
    , states :: [a]
    }

loop :: Eq a => SimulationState a -> IO ()
loop state = do
    event <- getEvent
    case event of
        Quit -> SDL.quit
        Insert x y -> loop state { space = (space state //
            [((y `div` (cellSize state), x `div` (cellSize state)),
             states state !! accColor state)])}
        NextColor -> loop state { accColor = (accColor state + 1) `mod` length (states state) }
        StartStop -> loop state { running = not (running state) }
        No -> do
            draw state
            threadDelay $ 10^5 * 2
            newSpace <- if running state
                then update (space state) (updateCell state)
                else return (space state)
            loop state { space = newSpace }
    where getEvent = SDL.pollEvent >>= \e -> case e of
            SDL.NoEvent -> return No
            SDL.Quit -> return Quit
            SDL.MouseButtonDown x y SDL.ButtonLeft  -> return $ Insert (fromIntegral x) (fromIntegral y)
            SDL.MouseButtonDown _ _ SDL.ButtonRight -> return NextColor
            SDL.KeyDown (SDL.Keysym SDL.SDLK_SPACE _ _) -> return StartStop
            _ -> getEvent

draw :: Eq a => SimulationState a -> IO ()
draw state = do
    SDL.fillRect (screen state) Nothing (SDL.Pixel 0)
    -- let pixelFormat = SDL.surfaceGetPixelFormat screen
    let (_, (maxRow, maxCol)) = bounds $ space state
    forM_ [0..maxRow] $ \row ->
        forM_ [0..maxCol] $ \col -> do
            let color = (colors state) (space state ! (row, col))
            let top = cellSize state * row
            let left = cellSize state * col
            Draw.box (screen state) (SDL.Rect left top (left + cellSize state) (top + cellSize state)) color
    SDL.flip (screen state)

------------------- creating spaces -----------------
randomSpace :: Int -> Int -> [a] -> IO (Space a)
randomSpace height width cellStateDist = fmap
    (listArray ((0, 0), (height-1, width-1)) . map (cellStateDist !!))
    (replicateM (width*height) (randomRIO (0, length cellStateDist - 1)))

initSpaceWithDefault :: a -> Int -> Int -> [((Int, Int), a)] -> Space a
initSpaceWithDefault defaultValue spaceWidth spaceHeight initCells = listArray
    ((0, 0), (spaceWidth - 1, spaceHeight - 1))
    (replicate (spaceWidth*spaceHeight) defaultValue) // initCells

initSpace = initSpaceWithDefault (0 :: Int)

--------------------- updating and rules ----------------
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
