module GUI (
    Color,
    white, black, grey, red, green, blue, cyan, brown, yellow, orange,
    runCellularAutomata2D) where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Primitives as Draw
import Control.Monad (void)
import Control.Concurrent (threadDelay)
import Data.Bits
import Data.Word (Word8)

import CellularAutomata2D

-- | Colors for the different cell values.
-- | On my machine mapRGBA didn't work, so I created my own colors
-- | using bitoperations.
type Color = SDL.Pixel

getColorFromRGB255 :: Word8 -> Word8 -> Word8 -> Color
getColorFromRGB255 r g b = SDL.Pixel $
    shiftL (fromIntegral r) 24 .|.
    shiftL (fromIntegral g) 16 .|.
    shiftL (fromIntegral b) 8  .|.
    255

red, green, blue, yellow, cyan, brown, orange, black, white, grey :: Color
red = getColorFromRGB255 255 0 0
green = getColorFromRGB255 0 255 0
blue = getColorFromRGB255 0 0 255
black = getColorFromRGB255 0 0 0
white = getColorFromRGB255 255 255 255
grey = getColorFromRGB255 100 100 100
yellow = getColorFromRGB255 255 255 0
cyan = getColorFromRGB255 0 255 255
brown = getColorFromRGB255 165 42 42
orange = getColorFromRGB255 255 165 0

targetScreenWidth :: Int
targetScreenWidth = 500

-- | Runs a GUI for a 2d cellular automata using
-- | a given starting space, a list of cell states witch can be used
-- | by the user to edit the space, a function witch takes a cell states
-- | and returns a color to draw the individual cells and a rule that is
-- | used to updated the space.
-- | The user can press space to start and stop the simulation of the automata.
-- | He can also edit the space by clicking into a cell witch goes to the next state.
runCellularAutomata2D :: (Space s, Eq a) => s a -> [a] -> (a -> Color) ->
                                 Rule s a -> IO ()
runCellularAutomata2D space states colors updateCell = do
    let (spaceHeight, spaceWidth) = getSpaceSize space
    let actualCellSize = targetScreenWidth `div` spaceWidth
    let screenWidth = spaceWidth * actualCellSize
    let screenHeight = spaceHeight * actualCellSize
    SDL.init []
    screen <- SDL.setVideoMode screenWidth screenHeight 32 [SDL.DoubleBuf]
    loop $ SimulationState screen colors actualCellSize
        updateCell space 0 False states

data PrivateEvent
    = No
    | Quit
    | Insert Int Int
    | NextColor
    | StartStop
    deriving (Show, Eq)

data SimulationState s a = SimulationState
    { _screen :: SDL.Surface
    , _colors :: a -> SDL.Pixel
    , cellSize :: Int
    , updateCellFn :: Rule s a
    , _space :: s a
    , accColor :: Int
    , running :: Bool
    , possibleStates :: [a]
    }

loop :: (Eq a, Space s) => SimulationState s a -> IO ()
loop state = do
    event <- getEvent
    case event of
        Quit -> SDL.quit
        Insert x y ->
            let cellIndex = (y `div` cellSize state, x `div` cellSize state) in
                          loop state { _space = setCell (_space state)
                            (next (getCell cellIndex $ _space state)) cellIndex }
        NextColor -> loop state { accColor = (accColor state + 1) `mod`
            length (possibleStates state) }
        StartStop -> loop state { running = not (running state) }
        No -> do
            draw state
            threadDelay $ 2 * 10 ^ (5 :: Int)
            newSpace <- if running state
                then update (_space state) (updateCellFn state)
                else return (_space state)
            loop state { _space = newSpace }
    where
        getEvent = SDL.pollEvent >>= \e -> case e of
            SDL.NoEvent -> return No
            SDL.Quit -> return Quit
            SDL.MouseButtonDown x y SDL.ButtonLeft  ->
                return $ Insert (fromIntegral x) (fromIntegral y)
            SDL.MouseButtonDown _ _ SDL.ButtonRight -> return NextColor
            SDL.KeyDown (SDL.Keysym SDL.SDLK_SPACE _ _) -> return StartStop
            _ -> getEvent
        next x = head $ tail $ dropWhile (/= x) $ cycle (possibleStates state)

draw :: (Space s, Eq a) => SimulationState s a -> IO ()
draw state = do
    SDL.fillRect (_screen state) Nothing (SDL.Pixel 0)
    forSpace (_space state) $ \(row, col) cell -> do
        let color = _colors state cell
        let top = cellSize state * row
        let left = cellSize state * col
        void $ Draw.box (_screen state) (SDL.Rect left top
            (left + cellSize state) (top + cellSize state)) color
    SDL.flip (_screen state)
