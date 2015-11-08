module GUI (
    Color,
    white, black, grey, red, green, blue, cyan, brown, yellow, orange,
    getColorFromRGB255,
    runCellularAutomata2D) where

import Control.Monad (void, when)
import Control.Concurrent (threadDelay)
import Data.Bits
import Data.Word (Word8)

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Primitives as Draw

import CellularAutomata2D

-- | Colors for the different cell values.
-- On my machine mapRGBA didn't work, so I created my own colors
-- using bitoperations.
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

targetScreenSize :: Int
targetScreenSize = 500

-- | Runs a GUI for a 2d cellular automata using
-- a given starting space, a list of cell states witch can be used
-- by the user to edit the space, a function witch takes a cell states
-- and returns a color to draw the individual cells and a rule that is
-- used to updated the space.
-- The user can press space to start and stop the simulation of the automata.
-- He can also edit the space by clicking into a cell witch goes to the next state.
runCellularAutomata2D :: (Space s, Eq a) => s a -> [a] -> (a -> Color) ->
                                 Rule a -> IO ()
runCellularAutomata2D space states colors updateCell = do
    let (spaceHeight, spaceWidth) = getSpaceSize space
    let cellSize' = if spaceHeight > spaceWidth
           then targetScreenSize `div` spaceHeight
           else targetScreenSize `div` spaceWidth
    let screenHeight = cellSize' * spaceHeight
    let screenWidth  = cellSize' * spaceWidth
    SDL.init []
    screen <- SDL.setVideoMode screenWidth screenHeight 32 [SDL.DoubleBuf]
    loop $ SimulationState screen colors cellSize'
        updateCell space 0 False states 3 0 0 1
        (div screenWidth 2) (div screenHeight 2)
        [] False

data Event
    = No
    | Quit
    | StopInserting
    | Insert Int Int
    | NextColor
    | StartStop
    | Home
    | GoLeft | GoRight | GoUp | GoDown
    | SoomIn | SoomOut
    deriving (Show, Eq)

data SimulationState s a = SimulationState
    { _screen :: SDL.Surface
    , _colors :: a -> SDL.Pixel
    , cellSize :: Int
    , updateCellFn :: Rule a
    , _space :: s a
    , accColor :: Int
    , running :: Bool
    , possibleStates :: [a]
    , _fps :: Int
    , transX, transY :: Int -- ^ number of cells
    , zoom :: Float
    , halfWidth, halfHeight :: Int
    , inserted :: [(Int, Int)]
    , inserting :: Bool
    }

loop :: (Eq a, Space s) => SimulationState s a -> IO ()
loop state = do
    start <- SDL.getTicks
    event <- getEvent
    case event of
        Quit -> SDL.quit
        StopInserting -> loop state { inserting = False, inserted = [] }
        Insert x y ->
            let cellIndex = (y `div` cellSize state, x `div` cellSize state) in
                if cellIndex `notElem` inserted state
                   then loop state { _space = setCell (_space state) cellIndex
                                                      (next (flip getCell cellIndex $ _space state)),
                                     inserted = cellIndex : inserted state,
                                     inserting = True }
                   else loop state
        NextColor -> loop state { accColor = (accColor state + 1) `mod`
            length (possibleStates state) }
        StartStop -> loop state { running = not (running state) }
        GoLeft -> loop state { transX = transX state + 1 }
        GoRight -> loop state { transX = transX state - 1 }
        GoUp -> loop state { transY = transY state + 1 }
        GoDown -> loop state { transY = transY state - 1 }
        SoomIn -> loop state { zoom = zoom state + 0.25 }
        SoomOut -> loop state { zoom = zoom state - 0.25 }
        Home -> loop state { transX = 0, transY = 0, zoom = 1, accColor = 0 }
        No -> do
            draw state
            newSpace <- if running state
                then update (_space state) (updateCellFn state)
                else return (_space state)
            stop <- SDL.getTicks
            let toDelay = 1 / realToFrac (_fps state) - realToFrac (stop - start) / 1000 :: Double
            when (toDelay > 0) $ threadDelay $ round $ 10^(5::Int) * toDelay
            loop state { _space = newSpace }
    where
        getEvent = SDL.pollEvent >>= \e -> case e of
            SDL.NoEvent -> return No
            SDL.Quit -> return Quit
            SDL.MouseButtonDown x y SDL.ButtonLeft -> return $ Insert (fromIntegral x) (fromIntegral y)
            SDL.MouseButtonUp _ _ SDL.ButtonLeft -> return StopInserting
            SDL.MouseMotion x y _ _ -> if inserting state then return $ Insert (fromIntegral x) (fromIntegral y) else getEvent
            SDL.MouseButtonDown _ _ SDL.ButtonRight -> return NextColor
            SDL.KeyDown (SDL.Keysym SDL.SDLK_SPACE _ _) -> return StartStop
            SDL.KeyDown (SDL.Keysym SDL.SDLK_LEFT _ _) -> return GoLeft
            SDL.KeyDown (SDL.Keysym SDL.SDLK_RIGHT _ _) -> return GoRight
            SDL.KeyDown (SDL.Keysym SDL.SDLK_UP _ _) -> return GoUp
            SDL.KeyDown (SDL.Keysym SDL.SDLK_DOWN _ _) -> return GoDown
            SDL.KeyDown (SDL.Keysym SDL.SDLK_PLUS _ _) -> return SoomIn
            SDL.KeyDown (SDL.Keysym SDL.SDLK_MINUS _ _) -> return SoomOut
            SDL.KeyDown (SDL.Keysym SDL.SDLK_h _ _) -> return Home
            _ -> getEvent
        next x = tail (dropWhile (/= x) $ cycle (possibleStates state)) !! accColor state

draw :: (Space s, Eq a) => SimulationState s a -> IO ()
draw state = do
    SDL.fillRect (_screen state) Nothing (SDL.Pixel 0)
    forSpace (_space state) $ \(row, col) cell -> do
        let color = _colors state cell
        let top = cellSize state * row + transY state * cellSize state
        let left = cellSize state * col + transX state * cellSize state
        void $ Draw.box
            (_screen state)
            (SDL.Rect
                (halfWidth state + round (zoom state * fromIntegral (left - halfWidth state)))
                (halfHeight state + round (zoom state * fromIntegral (top - halfHeight state)))
                (halfWidth state + round (zoom state * fromIntegral (left + cellSize state - halfWidth state)))
                (halfHeight state + round (zoom state * fromIntegral (top + cellSize state + halfHeight state))))
            color
    SDL.flip (_screen state)
