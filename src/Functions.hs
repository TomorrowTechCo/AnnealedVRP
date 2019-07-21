{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Functions where

import           Control.Monad.Reader
import           Data.List
import           Data.List.Index
import           Data.Matrix
import qualified Data.Text            as T
import           Data.Time
import qualified Data.Vector          as V
import           System.Random
import           Types

-- la lista de camiones.
-- TODO: change this for Reader Monad
truckList :: [Truck]
truckList = [Truck "ABC123" 30 20, Truck "XYZ789" 30 25]

truckList' :: V.Vector Truck
truckList' = V.fromList truckList

-- la matriz de distancias.
-- TODO: change this for Reader Monad
routeList :: Matrix Float
routeList = matrix 10 10 (\(i, j) -> fromIntegral $ abs (i-j))

parseDate :: String -> LocalTime
parseDate = parseTimeOrError True defaultTimeLocale "%d %m %Y %H:%M:%S"

-- La lista de clientes.
-- TODO: this goes into the Reader Monad
clientList :: [Client]
clientList = [Client 123 "Juan" 10.0 $ parseDate "10 02 2019 10:00:00"
            , Client 124 "Maria" 15.0 $ parseDate "11 02 2019 11:00:00"]

clientList' :: V.Vector Client
clientList' = V.fromList clientList

-- tiempo máximo
-- TODO: put this on the reader monad
tmax :: Time
tmax = 50

-- Generate a sample solution by randomly assigning clients to the trucks.
initSol' :: Solution -> [Id Client] -> StdGen -> Solution
initSol' !sol !clients rand = case clients of
  []     -> sol
  (x:xs) -> initSol' (insertLoc i x sol) xs rand'
  where
    (i, rand') = randomR (1 :: Int, length clients) rand
    insertLoc :: Int -> Int -> Solution -> Solution
    insertLoc i loc = modifyAt i (modifyAt 0 (loc:))

-- generates an empty solution from the number of trucks.
emptySol :: (MonadReader Config m, MonadIO m) => m Solution
emptySol = do
  listTrucks <- asks getTrucks
  return $! take (length listTrucks) $ repeat [[]]

initSol :: (MonadReader Config m, MonadIO m) => m (StdGen -> Solution)
initSol = do
  empty <- emptySol
  clientList <- asks getClients
  return $! initSol' empty $ V.toList $ fmap getId clientList

-- Compute the total cost of a truck's route
truckCost' :: Cost -> Id Client -> [Id Client] -> Cost
truckCost' cost last [] = cost + getElem last 1 routeList
truckCost' cost last (x:xs) = truckCost' (cost + getElem last x routeList) x xs

truckCost :: [Id Client] -> Cost
truckCost = truckCost' 0 1

-- Energy function, measuring the total travel distance for the current
-- solution.
totalCost' :: Solution -> Cost -> Cost
totalCost' [] acc     = acc
totalCost' (x:xs) acc = totalCost' xs (truckCost (x!!1) + acc)

totalCost :: EnergyFunction Solution
totalCost sol = totalCost' sol 0

{-
  Faster checking of cost by taking into account current, already calculated
  cost and computing the difference.
  TODO: overload the operator (!!) so whenever it's out of bounds, it assumes
  the travel is between the client and Depot, and returns the depot's id (1
  for the time being).
-}
updCost :: Cost -> Solution -> Modif -> Cost
updCost curCost sol (MoveTwo a (m1,m2) b (n1,n2)) =
  curCost - (getElem (sol!!m1!!0!!(m2-1)) a routeList)
    - (getElem a (sol!!m1!!0!!(m2+1)) routeList)
    + (getElem (sol!!m1!!0!!(m2-1)) b routeList)
    + (getElem b (sol!!m1!!0!!(m2+1)) routeList)
    - (getElem (sol!!n1!!0!!(n2-1)) b routeList)
    - (getElem b (sol!!n1!!0!!(n2+1)) routeList)
    + (getElem (sol!!n1!!0!!(n2-1)) a routeList)
    + (getElem a (sol!!n1!!0!!(n2+1)) routeList)

updCost curCost sol (MoveOne a (m1,m2) (n1,n2))   = _ -- TODO: implement this

-- Temperature function, that'll give us the current temp based on the distance
-- between the current and ending time.
curTemp :: TemperatureFunction
curTemp curr = 50
  * (exp (0.0 - (5.0 * ((fromIntegral tmax) / (fromIntegral curr)))) :: Float)

-- Motion function, that swaps two random clients of position.
-- note: if only one client is to be moved, place it before the selected
-- position.
swapClient :: Modif -> Solution -> Solution
swapClient (MoveTwo a (m1,m2) b (n1,n2)) sol =
  (modifyAt n1 (modifyAt 0 (setAt n2 a)))
  (modifyAt m1 (modifyAt 0 (setAt m2 b)) sol)
swapClient (MoveOne a (m1,m2) (n1,n2)) sol =
  (modifyAt m1 (modifyAt 0 (deleteAt m2)))
  (modifyAt n1 (modifyAt 0 (insertAt n2 a)) sol)

{-
  simplified motion function that only returns the clients to be swapped,
  plus their locations in the current solution. May occasionally move a
  client alone, changing the size of the truck routes.
-}
getSwap :: StdGen -> Solution -> Modif
getSwap rand sol = if twoP
  then MoveTwo (sol!!m1!!0!!m2) (m1,m2) (sol!!n1!!0!!n2) (n1,n2)
  else MoveOne (sol!!m1!!0!!m2) (m1,m2) (n1,n2)
  where
    -- (m1, rand0) = randomB sol rand
    -- (m2, rand1) = randomB (sol !! m1 !! 0) rand0
    -- (twoP :: Bool, rand2) = random rand1
    -- (n1, rand3) = randomB sol rand2
    -- (n2, rand4) = randomB (sol !! n1 !! 0) rand3
    (rand', [m1, m2, n1, n2]) = (rand, []) ~# randomB sol
      ~# randomB (sol !! m1 !! 0)
      ~# randomB sol
      ~# randomB (sol !! n1 !! 0)
    (twoP :: Bool, rand'') = random rand'

randomB :: [a] -> StdGen -> (Int, StdGen)
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
  else (ceiling . exp . negate) $ (cCurrent - cChanged) / curTemp

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
  trucks <- asks getTrucks
  routes <- asks getRoutes
  return $! if null $ V.filter
               (\a -> a > (1/2)*(foldl' max 0 (fmap autonomy trucks)))
               $ getRow 1 routes
            then False
            else True

{- Main loop. -}
anneal :: StdGen -> Time -> Solution -> Solution
anneal rand !time !sol = if (time >= tmax)
  then sol
  else anneal rand' (time - 1) $
    if (calcProb (totalCost sol)
        (totalCost (swapClient rand sol))
        (curTemp time)) > (randomVal)
    then (swapClient rand sol)
    else sol
  where
    (randomVal, rand') = randomR (1, 100) rand

{- Executable. -}
runApp :: IO ()
runApp = do
  -- get random generator
  randGen <- getStdGen

  -- create configuration
  let conf = Config truckList' routeList clientList' tmax
      x  = return 5 :: Reader Config Int

  -- verify if it's possible to solve
  valid <- runReaderT checkValues conf

  -- generate an initial solution
  init' <- runReaderT initSol conf
  init <- return $! init' randGen



  result <- return $ runReader x conf
  putStrLn $ "The solution is :" ++ show result
