import Control.Monad
import Control.Applicative
import Data.Array
import Data.List

import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.Modifiers

import CellularAutomata2D
import Files

main :: IO ()
main = do
    -- unit tests
    runTestTT $ TestList
        [TestCase $ assertEqual "moor neighborhood" (sort moorIndexDeltas)
                                                    (sort [(-1,-1), (0,-1), (0,1), (1,0), (1,1), (0,1), (-1,1), (1,0)])
        ,TestCase $ assertEqual "von neumann" (sort neumannIndexDeltas)
                                              (sort [(0,1),(0,-1),(1,0),(-1,0)])
        ]
    -- property tests
    -- get and set
    quickCheck (\space index value -> (value :: Int) == getCell (setCell space index value) index)
    -- index mapping
    quickCheck (\space -> let is = [(row, col) | let (h, w) = getSpaceSize (space :: Torus Int), row <- [0..h-1], col <- [0..w-1]] in
                              map (remapIndex space) is == is)
    quickCheck (\space index -> let (row, col) = remapIndex (space :: Torus Int) index
                                    (h, w) = getSpaceSize space in
                                    row >= 0 && row < h && col >= 0 && col < w)
    -- setCells
    quickCheck (\space cells -> let space' = setCells (space :: Torus Int) cells in
                                    map (getCell space' . fst) cells == map snd cells)
    -- initSpace and getSpaceSize
    quickCheck (\size -> size == getSpaceSize (initSpace size (const (1 :: Int))))

    space <- randomSpace (10, 10) [0,1]
    saveToFile space "test.space"
    space' <- loadFromFile "test.space"
    putStrLn $ if space == space' then "ok, io works" else "failed to reload file"


instance Arbitrary a => Arbitrary (Torus a) where
    arbitrary = do
        n <- getSmall <$> arbitrary
        m <- getSmall <$> arbitrary
        xs <- vector (n*m)
        return $ Torus $ listArray ((0, 0), (n - 1, m - 1)) xs

