module Utils where

import           Data.Matrix
import           System.Random

-- new operator
(~#) :: (StdGen, [a]) -> (StdGen -> (a, StdGen)) -> (StdGen, [a])
(~#) (rand, l) func = (snd $ func rand, (fst $ func rand):l)

func :: StdGen -> (Int, StdGen)
func = randomR (0::Int, 1)
func2 = randomR (0::Int, 2)

testFunc :: IO ()
testFunc = do
  newRand <- getStdGen
  dinero <- return $! (newRand, []) ~# func ~# func2
  putStr $ show dinero

-- utility operator to extract several values of a matrix.
(#>) :: (Matrix Int, Int) -> (Int, Int) -> (Matrix Int, Int)
(#>) (mtrx, a) (x, y) = (mtrx, a + (getElem x y mtrx))
