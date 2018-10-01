module OneDim where

import Data.Bits

import CellularAutomata2D
import GUI

initSpace1D :: Int -> [a] -> Torus (Maybe a)
initSpace1D steps initialSpace1D = setCells emptySpace (zip fstRow $ map Just initialSpace1D)
    where emptySpace = initSpace (steps, length initialSpace1D) (const $ Nothing)
          fstRow = zip (repeat 0) [0..length initialSpace1D - 1]

make1DRule :: Int -> ([a] -> a) -> Rule (Maybe a)
make1DRule radius rule1D = Rule
    { ruleNeighborhoodDeltas = zip (repeat $ -1) [-radius..radius]
    , ruleFunction = \self maybeNeighborhood -> case self of
        Just state -> return (Just state)
        Nothing -> case sequence maybeNeighborhood of
            Just neighborhood -> return $ Just $ rule1D neighborhood
            Nothing -> return Nothing
    }

instance Cell a => Cell (Maybe a) where
    getColor (Just s) = getColor s
    getColor Nothing = grey

    getSuccState = fmap getSuccState

elementaryCellularAutomata :: Int -> Rule (Maybe Bool)
elementaryCellularAutomata ruleNumber = make1DRule 1 (\[l, c, r] ->
            0 /= (ruleNumber .&. (shiftL 1 (4 * boolToInt l + 2 * boolToInt c + boolToInt r))))
        where boolToInt True = 1
              boolToInt False = 0

singleCell :: Int -> [Bool]
singleCell size = map isMidpoint [1..size]
    where isMidpoint = (== div size 2)

main :: IO ()
main = runCellularAutomata2D (elementaryCellularAutomata 110) (initSpace1D 100 $ singleCell 100) >> return ()
