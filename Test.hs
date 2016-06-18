import Control.Monad
import Control.Applicative
import Control.Arrow (first)
import Data.Array
import Data.List
import Data.Function (on)

import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.Modifiers

import Test.Framework as TF
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import CellularAutomata2D
import Files
import qualified GameOfLife

main :: IO ()
main = defaultMain tests

tests :: [TF.Test]
tests = [
    testGroup "neighborhoods" [
        testCase "moor" testMoorNeighborhood,
        testCase "neumann" testNeumannNeighborhood
        ],
    testGroup "stepping" [
        testCase "game of life" testGOLStep
        ],
    testGroup "remapping/torus" [
        testProperty "all indices in the data array remap to themself" propSimpleIndexNotRemaped,
        testProperty "all indices should be remaped into the data array" propReMapInRange
        ],
    testGroup "indxing" [
        testProperty "cells you set should contain the given content" propGetSet,
        testProperty "if you set the complete space, is should contain the given content" propSetCells
        ],
    testGroup "fileio" [
        testCase "If you save a space to a file and you load it back, there should be the same." testLoadSave
        ]
    ]

-- | checks if the update in the game of life with a glieder is correct.
testGOLStep :: Assertion
testGOLStep = do
    resultSpace' <- update GameOfLife.golRule testSpace
    resultSpace' @?= resultSpace
    where testSpace = fromMatrix [[0, 0, 1, 0, 0],
                                  [1, 0, 1, 0, 0],
                                  [0, 1, 1, 0, 0],
                                  [0, 0, 0, 0, 0],
                                  [0, 0, 0, 0, 0]]
          resultSpace = fromMatrix [[0, 1, 0, 0, 0],
                                    [0, 0, 1, 1, 0],
                                    [0, 1, 1, 0, 0],
                                    [0, 0, 0, 0, 0],
                                    [0, 0, 0, 0, 0]]

-- | check if the von neumann neighborhood is correct
testNeumannNeighborhood :: Assertion
testNeumannNeighborhood = sort neumannIndexDeltas @?= sort [(0,1),(0,-1),(1,0),(-1,0)]

-- | check if the moor neighborhood is correct
testMoorNeighborhood :: Assertion
testMoorNeighborhood = sort moorIndexDeltas @?= sort [(-1,-1), (0,-1), (0,1), (-1,0), (1,0), (1,1), (-1,1), (1,-1)]

type Index = (NonNegative Int, NonNegative Int)

getIndex :: Index -> (Int, Int)
getIndex (a, b) = (getNonNegative a, getNonNegative b)

type Shape = (Positive Int, Positive Int)

getShape :: Shape -> (Int, Int)
getShape (a, b) = (getPositive a, getPositive b)

-- | checks than you get the same value back from a place where you had set it.
propGetSet :: Torus Int -> Index -> Int -> Bool
propGetSet space index value = value == getCell (setCell space (getIndex index) value) (getIndex index)

-- | All indices in the data array should be remaped to them self.
propSimpleIndexNotRemaped :: Torus Int -> Bool
propSimpleIndexNotRemaped space = map (remapIndex space) is == is
    where is = [(row, col) | let (h, w) = getSpaceSize (space :: Torus Int), row <- [0..h-1], col <- [0..w-1]]

-- | All remaped indices should be remaped to indices in the data array
propReMapInRange :: Torus Int -> Index -> Bool
propReMapInRange space index = row >= 0 && row < h && col >= 0 && col < w
    where (row, col) = remapIndex (space :: Torus Int) (getIndex index)
          (h, w) = getSpaceSize space

-- | If you set a lot of cells and then get all, there should be the same.
propSetCells :: Torus Int -> [(Index, Int)] -> Bool
propSetCells space cells = map (getCell space' . getIndex . fst) cells' == map snd cells'
    where space' = setCells space (map (first getIndex) cells')
          cells' = nubBy (\a b -> remapIndex space (getIndex $ fst a) == remapIndex space (getIndex $ fst b)) cells

-- | If you init a new cell with a given size, it should have this size.
propInitSpaceGetSpaceSize :: Shape -> Bool
propInitSpaceGetSpaceSize size = getShape size == getSpaceSize (initSpace (getShape size) (const (1 :: Int)))

-- | If you save a space to a file and you load it back, there should be the same.
testLoadSave :: Assertion
testLoadSave = do
    space <- randomSpace (10, 10) [0,1]
    saveToFile space "test.space"
    space' <- loadFromFile "test.space"
    space @?= space'


instance Arbitrary a => Arbitrary (Torus a) where
    arbitrary = do
        n <- getSmall . getPositive <$> arbitrary
        m <- getSmall . getPositive <$> arbitrary
        xs <- vector (n*m)
        return $ Torus $ listArray ((0, 0), (n - 1, m - 1)) xs
