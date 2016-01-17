module GUI (
    Color,
    getColorFromRGB255,
    white, black, grey, red, green, blue, cyan, brown, yellow, orange,
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

-- | get a color value from a red, a green and a blue component of type Word8 (0..255)
getColorFromRGB255 :: Word8 -> Word8 -> Word8 -> Color
getColorFromRGB255 r g b = SDL.Pixel $
    shiftL (fromIntegral r) 24 .|.
    shiftL (fromIntegral g) 16 .|.
    shiftL (fromIntegral b) 8  .|.
    255

-- | basic colors
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

-- | the size of the display window we try to get
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
    -- compute our window dimensions
    let (spaceHeight, spaceWidth) = getSpaceSize space
    let cellSize' = if spaceHeight > spaceWidth
           then targetScreenSize `div` spaceHeight
           else targetScreenSize `div` spaceWidth
    let screenHeight = cellSize' * spaceHeight
    let screenWidth  = cellSize' * spaceWidth
    -- setup SDL and open a window
    SDL.init []
    screen <- SDL.setVideoMode screenWidth screenHeight 32 [SDL.DoubleBuf]
    -- start the game loop
    loop $ SimulationState screen colors cellSize'
        updateCell space 0 False states 3 0 0 1
        (div screenWidth 2) (div screenHeight 2)
        [] False 0 0

data SimulationState a = SimulationState
    { getScreen :: SDL.Surface
    , getColor :: a -> SDL.Pixel
    , cellSize :: Int
    , updateCellFn :: Rule a
    , getSpace :: Torus a
    , accColor :: Int
    , running :: Bool
    , possibleStates :: [a]
    , getFPS :: Int
    , transX, transY :: Int -- ^ number of cells
    , zoom :: Float
    , halfWidth, halfHeight :: Int
    , inserted :: [(Int, Int)]
    , inserting :: Bool
    , moveX :: Int, moveY :: Int
    }

-- | the game loop
loop :: (Eq a) => SimulationState a -> IO ()
loop state = do
    start <- SDL.getTicks
    -- get an event and process it
    event <- SDL.pollEvent
    case event of
        SDL.Quit -> SDL.quit
        SDL.MouseButtonUp _ _ SDL.ButtonLeft -> loop state { inserting = False, inserted = [] } -- stop changing cell states
        SDL.MouseMotion x y _ _
            | inserting state -> insert state x y -- inserting new cells
            | otherwise -> loop state
        SDL.MouseButtonDown x y SDL.ButtonLeft -> insert state { inserting = True } x y -- start changing cells
        SDL.MouseButtonDown _ _ SDL.ButtonRight -> loop state { accColor = (accColor state + 1) `mod`
            length (possibleStates state) }
        -- toogle running (update of the world)
        SDL.KeyDown (SDL.Keysym SDL.SDLK_SPACE _ _) -> loop state { running = not (running state) }
        -- move the users view on the world
        SDL.KeyDown (SDL.Keysym SDL.SDLK_LEFT _ _) -> loop state { moveX = 1 }
        SDL.KeyDown (SDL.Keysym SDL.SDLK_RIGHT _ _) -> loop state { moveX = -1 }
        SDL.KeyDown (SDL.Keysym SDL.SDLK_UP _ _) -> loop state { moveY = 1 }
        SDL.KeyDown (SDL.Keysym SDL.SDLK_DOWN _ _) -> loop state { moveY = -1 }
        SDL.KeyUp (SDL.Keysym SDL.SDLK_LEFT _ _) -> loop state { moveX = 0 }
        SDL.KeyUp (SDL.Keysym SDL.SDLK_RIGHT _ _) -> loop state { moveX = 0 }
        SDL.KeyUp (SDL.Keysym SDL.SDLK_UP _ _) -> loop state { moveY = 0 }
        SDL.KeyUp (SDL.Keysym SDL.SDLK_DOWN _ _) -> loop state { moveY = 0 }
        -- zoom in/out
        SDL.KeyDown (SDL.Keysym SDL.SDLK_PLUS _ _) -> loop state { zoom = zoom state + 0.25 }
        SDL.KeyDown (SDL.Keysym SDL.SDLK_MINUS _ _) -> loop state { zoom = zoom state - 0.25 }
        -- reset our view (no zoom or translation)
        SDL.KeyDown (SDL.Keysym SDL.SDLK_h _ _) -> loop state { transX = 0, transY = 0, zoom = 1, accColor = 0 }
        -- advance for one generation (only if we aren't running)
        SDL.KeyDown (SDL.Keysym SDL.SDLK_RETURN _ _) -> if running state then loop state else
                update (getSpace state) (updateCellFn state) >>= \space' ->
                  loop state { getSpace = space' }
        -- donw processing the events
        SDL.NoEvent -> do
            draw state
            -- if we are running then update the world
            newSpace <- if running state
                then update (getSpace state) (updateCellFn state)
                else return (getSpace state)
            -- delay to get the right FPS
            stop <- SDL.getTicks
            let toDelay = 1 / realToFrac (getFPS state) - realToFrac (stop - start) / 1000 :: Double
            when (toDelay > 0) $ threadDelay $ round $ 10^(5::Int) * toDelay
            loop state { getSpace = newSpace
                       , transX = moveX state + transX state -- apply moves
                       , transY = moveY state + transY state }
        _ -> loop state
    where
        -- FIXME: if you have states witch do not appeare in the possibleStates list,
        -- next goes into an infinit loop.
        next x = tail (dropWhile (/= x) $ cycle (possibleStates state)) !! accColor state
        -- change the cell state at a given pixel coordinate
        insert state' x y = if not isOutside && cellIndex `notElem` inserted state'
                   then loop state' { getSpace = setCell (getSpace state') cellIndex
                                                      (next (flip getCell cellIndex $ getSpace state')),
                                     inserted = cellIndex : inserted state' }
                   else loop state'
                where cellIndex = ((floor (fromIntegral (fromIntegral y - halfHeight state)/zoom state) + halfHeight state) `div` cellSize state - transY state,
                                   (floor (fromIntegral (fromIntegral x - halfWidth state)/zoom state) + halfWidth state) `div` cellSize state - transX state)
                      isOutside = fst cellIndex < 0 || fst cellIndex >= fst (getSpaceSize $ getSpace state) ||
                                snd cellIndex < 0 || snd cellIndex >= snd (getSpaceSize $ getSpace state)

-- x -> col
-- col = ((x - w/2)/zoom + w/2)/cellSize - transX
-- col -> x
-- x = zoom*((col + transX)*cellSize - w/2) + w/2

-- | draw the state of the automata to the window
draw :: (Eq a) => SimulationState a -> IO ()
draw state = do
    -- clear the window
    SDL.fillRect (getScreen state) Nothing (SDL.Pixel 0)
    forSpace (getSpace state) $ \(row, col) cell -> do
        -- get the rect of the cell
        let top = cellSize state * row + transY state * cellSize state
        let left = cellSize state * col + transX state * cellSize state
        let rect = SDL.Rect
                (round (zoom state * fromIntegral (left - halfWidth state)) + halfWidth state)
                (round (zoom state * fromIntegral (top - halfHeight state)) + halfHeight state)
                (round (zoom state * fromIntegral (left + cellSize state - halfWidth state)) + halfWidth state)
                (round (zoom state * fromIntegral (top + cellSize state - halfHeight state)) + halfHeight state)
        -- draw cell
        let color = getColor state cell
        void $ Draw.box (getScreen state) rect color
        -- draw the grid
        void $ Draw.rectangle (getScreen state) rect black
    SDL.flip (getScreen state)
