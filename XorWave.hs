module XorWave where

import CellularAutomata2D
import GUI

data BitField = BitField { previous, current :: Bool } deriving (Show, Eq, Ord, Bounded)

instance Cell BitField where
    getColor bf = if current bf then black else white
    getSuccState = cycleEnum

instance Enum BitField where
    fromEnum (BitField False False) = 0
    fromEnum (BitField False True) = 1
    fromEnum (BitField True False ) = 2
    fromEnum (BitField True True) = 3

    toEnum 0 = (BitField False False)
    toEnum 1 = (BitField False True)
    toEnum 2 = (BitField True False)
    toEnum 3 = (BitField True True)

xor :: Bool -> Bool -> Bool
xor = (/=)

xorSum :: [Bool] -> Bool
xorSum = foldr (/=) False

makeSecondOrderRule :: (Bool -> Bool -> [Bool] -> Bool) -> Rule BitField
makeSecondOrderRule f = Rule neumannIndexDeltas (\self neighbors -> return $ BitField (current self) (f (current self) (previous self) (map current neighbors)))

xorWave :: Rule BitField
xorWave = makeSecondOrderRule (\currentSelf previousSelf neighbors -> previousSelf `xor` xorSum neighbors)

xorKleinGordon :: Rule BitField
xorKleinGordon = makeSecondOrderRule (\currentSelf previousSelf neighbors -> previousSelf `xor` currentSelf `xor` xorSum neighbors)

mainXorWave = randomSpace (100, 100) [minBound..maxBound] >>= runCellularAutomata2D xorWave >> return ()
mainXorKleinGordon = randomSpace (100, 100) [minBound..maxBound] >>= runCellularAutomata2D xorKleinGordon >> return ()
