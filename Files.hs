module Files where

import Control.Applicative ((<$>))

import CellularAutomata2D

loadFromFile :: (Read a) => FilePath -> IO (Torus a)
loadFromFile path = read <$> readFile path

saveToFile :: (Show a) => Torus a -> FilePath -> IO ()
saveToFile space path = writeFile path (show space)
