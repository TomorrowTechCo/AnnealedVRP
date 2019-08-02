{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Functions
  ( anneal
  , initSol
  , checkValues
  , totalCost ) where

import           Control.Monad.Reader
import           Data.List
import           Data.Matrix
import qualified Data.Sequence        as Sq
import qualified Data.Text            as T
import           Data.Time
import           Data.Time.Format
import qualified Data.Vector          as V
import           System.Random

import           Types
import           Utils

-- cuenta
account :: Account
account = Account 123 "Dinero SA" (Just "Dinero y algo mas" ) "calle falsa 123"

-- la matriz de distancias.
-- TODO: change this for Reader Monad
routeList :: Matrix Distance
routeList = matrix 10 10 (\(i, j) -> fromIntegral $ abs (i-j))

parseDate :: String -> LocalTime
parseDate = parseTimeOrError True defaultTimeLocale "%d %m %Y %H:%M:%S"

-- La lista de ubicaciones.
-- TODO: this goes into the Reader Monad
clientList :: V.Vector Location
clientList = V.fromList [Location
                         "Calle bogotá conjunto villahermosa casa 9"
                         10.0 75.0
                        , Location "Avenida Dinero calle 123"
                          15.0 70.0]

indexes' :: Int -> [Index]
indexes' 0 = []
indexes' n = n:(indexes' $ n - 1)

indexes n = (V.fromList . reverse) $ indexes' n

-- tiempo máximo
-- TODO: put this on the reader monad
tmax :: Time
tmax = 50

-- Generate a sample solution by randomly assigning clients to the trucks.
initSol' :: Solution -> [Index] -> StdGen -> Solution
initSol' !sol !clients rand = case clients of
  []     -> sol
  (x:xs) -> initSol' (insertLoc i x sol) xs rand'
  where
    (i, rand') = randomR (1, length clients) rand
    insertLoc :: Int -> Index -> Solution -> Solution
    insertLoc i client = Sq.adjust' (Sq.insertAt 0 client) i

-- generates an empty solution from the number of trucks.
emptySol :: (MonadReader Config m, MonadIO m) => m Solution
emptySol = do
  listTrucks <- asks forTrucks
  return $! Sq.fromList $ take (length listTrucks) $ repeat Sq.empty

initSol :: (MonadReader Config m, MonadIO m) => m (StdGen -> Solution)
initSol = do
  empty <- emptySol
  clientList <- asks forClients
  return $! initSol' empty $ V.toList $ clientList

-- Compute the total cost of a truck's route
truckCost' :: Cost -> Index -> Sq.Seq Index -> Cost
truckCost' cost last Sq.Empty = cost + getElem last 1 routeList
truckCost' cost last (x Sq.:<| xs) = truckCost' (cost + getElem last x routeList) x xs

truckCost :: Sq.Seq Index -> Cost
truckCost = truckCost' 0 1

-- Energy function, measuring the total travel distance for the current
-- solution.
totalCost' :: Solution -> Cost -> Cost
totalCost' Sq.Empty acc      = acc
totalCost' (x Sq.:<| xs) acc = totalCost' xs (truckCost x + acc)

totalCost :: EnergyFunction Solution
totalCost sol = totalCost' sol 0

{-
  Faster checking of cost by taking into account current, already calculated
  cost and computing the difference.
  DONE: overload the operator (!!) so whenever it's out of bounds, it assumes
  the travel is between the client and Depot, and returns the depot's id (1
  for the time being). (created new operator)
-}
updCost :: Cost -> Solution -> Matrix Distance -> Modif -> Cost
updCost curCost sol mtrx modif = case modif of
  (MoveTwo a (m1,m2) b (n1,n2)) ->
    curCost - get (loc m1 m2 (-)) a
    - get a (loc m1 m2 (+))
    + get (loc m1 m2 (-)) b
    + get b (loc m1 m2 (+))
    - get (loc n1 n2 (-)) b
    - get b (loc n1 n2 (+))
    + get (loc n1 n2 (-)) a
    + get a (loc n1 n2 (+))
  (MoveOne a (m1,m2) (n1,n2)) ->
    curCost - get (loc m1 m2 (-)) a
    - get a (loc m1 m2 (+))
    + get (loc m1 m2 (-)) (loc m1 m2 (+))
    - get (loc n1 n2 (-)) (loc n1 n2 (+))
    + get (loc n1 n2 (-)) a
    + get a (loc n1 n2 (+))
  where
    get x y = getElem x y mtrx
    (!+) :: Sq.Seq Index -> Int -> Index
    (!+) l i = if (i > (length l)||i < 0) then 1 else l `Sq.index` i
    loc x y sig = (sol `Sq.index` x !+ (sig y 1))

-- Temperature function, that'll give us the current temp based on the distance
-- between the current and ending time.
curTemp :: TemperatureFunction
curTemp curr = 50
  * (exp (0.0 - (5.0 * ((fromIntegral tmax) / (fromIntegral curr)))) :: Double)

-- Motion function, that swaps two random clients of position.
-- note: if only one client is to be moved, place it before the selected
-- position.
swapClient :: Modif -> Solution -> Solution
swapClient (MoveTwo a (m1,m2) b (n1,n2)) sol =
  (adjust'' n1 (Sq.update n2 a))
  (adjust'' m1 (Sq.update m2 b) sol)
  where
    adjust'' = flip Sq.adjust'
swapClient (MoveOne a (m1,m2) (n1,n2)) sol =
  (adjust'' m1 (Sq.deleteAt m2))
  (adjust'' n1 (Sq.insertAt n2 a) sol)
  where
    adjust'' = flip Sq.adjust'

{-
  simplified motion function that only returns the clients to be swapped,
  plus their locations in the current solution. May occasionally move a
  client alone, changing the size of the truck routes.
-}
getSwap :: StdGen -> Solution -> (Modif, StdGen)
getSwap rand sol = if twoP
  then (MoveTwo (sol `Sq.index` m1 `Sq.index` m2)
        (m1,m2) (sol `Sq.index` n1 `Sq.index` n2) (n1,n2), rand'')
  else (MoveOne (sol `Sq.index` m1 `Sq.index` m2) (m1,m2) (n1,n2), rand'')
  where
    (rand', [m1, m2, n1, n2]) = (rand, []) ~# randomB sol
      ~# randomB (sol `Sq.index` m1)
      ~# randomB sol
      ~# randomB (sol `Sq.index` n1)
    (twoP :: Bool, rand'') = random rand'

randomB :: (Sq.Seq a) -> StdGen -> (Int, StdGen)
randomB list = randomR (0, length list)
--

{-
  Transition probability function. Calculate the probability of the result
  passing, if it is worse than the last one. Then throw a die between 0 and 1
  and check the result.
-}
calcProb :: TransitionProbabilityFunction
calcProb cCurrent cChanged curTemp = if (cCurrent < cChanged)
  then 100
  else (ceiling . (*100) . exp . negate) $ (cCurrent - cChanged) / curTemp

{-
Checking:
  Revisar que sea posible resolver el problema:
  - que no haya clientes que estén a más la mitad de la autonomía más alta con la
    que cuente la flota.
  - Cualificado: que no haya envíos más grandes que la capacidad de carga más alta
    con la que cuente la flota.
-}
checkValues :: (MonadReader Config m, MonadIO m) => m Bool
checkValues = do
  trucks <- asks forTrucks
  routes <- asks forRoutes
  return $! not $ null $ V.filter
               (\a -> a > (1/2)*(foldl' max 0 (fmap vehicleAutonomy trucks)))
               $ getRow 1 routes

{- Main loop. -}
anneal :: StdGen -> Time -> Matrix Distance -> Cost -> Solution -> Solution
anneal rand !time mtrx !cost !sol = if (time >= tmax)
  then sol
  else anneal rand'' (time - 1) mtrx cost $
    if (calcProb cost
        (updCost cost sol mtrx solModif)
        (curTemp time)) > (randomVal)
    then (swapClient solModif sol)
    else sol
  where
    (randomVal, rand') = randomR (1, 100) rand
    (solModif, rand'') = getSwap rand' sol

{- Executable. -}
-- runApp :: IO ()
-- runApp = do
--   -- get random generator
--   randGen <- getStdGen

--   -- create configuration
--   let conf = Config truckList' routeList (indexes (length clientList)) tmax
--       x  = return 5 :: Reader Config Int

--   -- verify if it's possible to solve
--   valid <- runReaderT checkValues conf
--   if not valid
--   then putStrLn "at least one route is impossible, abort."
--   else do

--     -- generate an initial solution
--     init' <- runReaderT initSol conf
--     let init = init' randGen

--     -- calculate initial cost
--     let initcost = totalCost init

--     -- calculate
--     let finalSol = anneal randGen (forTMax conf) (forRoutes conf) initcost init

--     -- return result
--     putStrLn $ "the solution is: " ++ show finalSol
