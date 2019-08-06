{-# LANGUAGE FlexibleContexts #-}
module Utils where

import           Control.Monad.Reader
import           Data.Matrix
import           Database.Persist.Sql
import           System.Random
import           Types

-- new operator
(~#) :: (StdGen, [a]) -> (StdGen -> (a, StdGen)) -> (StdGen, [a])
(~#) (rand, l) func = (snd $ func rand, (fst $ func rand):l)

-- func :: StdGen -> (Int, StdGen)
-- func = randomR (0::Int, 1)
-- func2 = randomR (0::Int, 2)

-- testFunc :: IO ()
-- testFunc = do
--   newRand <- getStdGen
--   dinero <- return $! (newRand, []) ~# func ~# func2
--   putStr $ show dinero

-- utility operator to extract several values of a matrix.
(#>) :: (Matrix Int, Int) -> (Int, Int) -> (Matrix Int, Int)
(#>) (mtrx, a) (x, y) = (mtrx, a + (getElem x y mtrx))

-- function to run an op in the database.
runDB query = do
  pool <- asks forPool
  liftIO $ runSqlPool query pool
