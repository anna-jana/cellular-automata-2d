-- module Analysis where

import Data.Array

import CellularAutomata2D
import RockPaperScissors hiding (main)

evolution :: Int -> Rule a -> Torus a -> IO [Torus a]
evolution 0 _ _ = return []
evolution steps rule space = do
    next <- update rule space
    future <- evolution (steps - 1) rule next
    return $ next : future

histogram :: (Bounded a, Ix a, Eq a) => Torus a -> Array a Int
histogram (Torus space) = foldr up init (indices space)
    where init = listArray (minBound, maxBound) [0..]
          up idx hist = let s = space ! idx in hist // [(s, (hist ! s) + 1)]

main :: IO ()
main = do
    let initialSpace = initSpaceWithCells (50, 50) (RPSCell White maxLives)
            [((10, 10), RPSCell Blue maxLives), ((20, 20), RPSCell Green maxLives), ((30, 30), RPSCell Red maxLives)]
    history <- evolution 500 rockPaperScissorsRule initialSpace
    let histograms = map histogram (fmap (fmap color) history)
    writeFile "data.dat" $ unlines $ map (unwords . map show . elems) histograms
