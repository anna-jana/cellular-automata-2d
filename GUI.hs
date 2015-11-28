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
runCellularAutomata2D :: (Eq a) => Torus a -> [a] -> (a -> Color) ->
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
        [] False 0 0

data SimulationState a = SimulationState
    { _screen :: SDL.Surface
    , _colors :: a -> SDL.Pixel
    , cellSize :: Int
    , updateCellFn :: Rule a
    , _space :: Torus a
    , accColor :: Int
    , running :: Bool
    , possibleStates :: [a]
    , _fps :: Int
    , transX, transY :: Int -- ^ number of cells
    , zoom :: Float
    , halfWidth, halfHeight :: Int
    , inserted :: [(Int, Int)]
    , inserting :: Bool
    , moveX :: Int, moveY :: Int
    }

loop :: (Eq a) => SimulationState a -> IO ()
loop state = do
    start <- SDL.getTicks
    event <- SDL.pollEvent
    case event of
        SDL.Quit -> SDL.quit
        SDL.MouseButtonUp _ _ SDL.ButtonLeft -> loop state { inserting = False, inserted = [] }
        SDL.MouseMotion x y _ _ -> if inserting state
                                      then insert state x y
                                      else loop state
        SDL.MouseButtonDown x y SDL.ButtonLeft -> insert state { inserting = True } x y
        SDL.MouseButtonDown _ _ SDL.ButtonRight -> loop state { accColor = (accColor state + 1) `mod`
            length (possibleStates state) }
        SDL.KeyDown (SDL.Keysym SDL.SDLK_SPACE _ _) -> loop state { running = not (running state) }
        SDL.KeyDown (SDL.Keysym SDL.SDLK_LEFT _ _) -> loop state { moveX = 1 }
        SDL.KeyDown (SDL.Keysym SDL.SDLK_RIGHT _ _) -> loop state { moveX = -1 }
        SDL.KeyDown (SDL.Keysym SDL.SDLK_UP _ _) -> loop state { moveY = 1 }
        SDL.KeyDown (SDL.Keysym SDL.SDLK_DOWN _ _) -> loop state { moveY = -1 }
        SDL.KeyUp (SDL.Keysym SDL.SDLK_LEFT _ _) -> loop state { moveX = 0 }
        SDL.KeyUp (SDL.Keysym SDL.SDLK_RIGHT _ _) -> loop state { moveX = 0 }
        SDL.KeyUp (SDL.Keysym SDL.SDLK_UP _ _) -> loop state { moveY = 0 }
        SDL.KeyUp (SDL.Keysym SDL.SDLK_DOWN _ _) -> loop state { moveY = 0 }
        SDL.KeyDown (SDL.Keysym SDL.SDLK_PLUS _ _) -> loop state { zoom = zoom state + 0.25 }
        SDL.KeyDown (SDL.Keysym SDL.SDLK_MINUS _ _) -> loop state { zoom = zoom state - 0.25 }
        SDL.KeyDown (SDL.Keysym SDL.SDLK_h _ _) -> loop state { transX = 0, transY = 0, zoom = 1, accColor = 0 }
        SDL.KeyDown (SDL.Keysym SDL.SDLK_RETURN _ _) -> if running state then loop state else
                update (_space state) (updateCellFn state) >>= \space' ->
                  loop state { _space = space' }
        SDL.NoEvent -> do
            draw state
            newSpace <- if running state
                then update (_space state) (updateCellFn state)
                else return (_space state)
            stop <- SDL.getTicks
            let toDelay = 1 / realToFrac (_fps state) - realToFrac (stop - start) / 1000 :: Double
            when (toDelay > 0) $ threadDelay $ round $ 10^(5::Int) * toDelay
            loop state { _space = newSpace
                       , transX = moveX state + transX state
                       , transY = moveY state + transY state }
        _ -> loop state
    where
        -- FIXME: if you have states witch do not appeare in the possibleStates list,
        -- next goes into an infinit loop.
        next x = tail (dropWhile (/= x) $ cycle (possibleStates state)) !! accColor state
        insert state' x y =
            let cellIndex = (fromIntegral y `div` cellSize state', fromIntegral x `div` cellSize state') in
                if cellIndex `notElem` inserted state'
                   then loop state' { _space = setCell (_space state') cellIndex
                                                      (next (flip getCell cellIndex $ _space state')),
                                     inserted = cellIndex : inserted state' }
                   else loop state'

draw :: (Eq a) => SimulationState a -> IO ()
draw state = do
    SDL.fillRect (_screen state) Nothing (SDL.Pixel 0)
    forSpace (_space state) $ \(row, col) cell -> do
        let color = _colors state cell
        let top = cellSize state * row + transY state * cellSize state
        let left = cellSize state * col + transX state * cellSize state
        let rect = SDL.Rect
                (round (zoom state * fromIntegral (left - halfWidth state)) + halfWidth state)
                (round (zoom state * fromIntegral (top - halfHeight state)) + halfHeight state)
                (round (zoom state * fromIntegral (left + cellSize state - halfWidth state)) + halfWidth state)
                (round (zoom state * fromIntegral (top + cellSize state - halfHeight state)) + halfHeight state)
        void $ Draw.box (_screen state) rect color
        void $ Draw.rectangle (_screen state) rect black
    SDL.flip (_screen state)
