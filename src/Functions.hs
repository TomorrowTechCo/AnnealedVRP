{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
module Functions where

import           Data.List
import           Data.List.Index
import           Data.Matrix
import qualified Data.Text       as T
import           Data.Time
import qualified Data.Vector     as V
import           System.Random
import           Types

-- la lista de camiones.
-- TODO: change this for Reader Monad
truckList :: [Truck]
truckList = [Truck "ABC123" 30 20, Truck "XYZ789" 30 25]

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

-- tiempo máximo
tmax :: Time
tmax = 50

-- Generate a sample solution by randomly assigning clients to the trucks.
initSol :: StdGen -> [Id Client] -> Solution -> Solution
initSol rand !clients !sol = case clients of
  []     -> sol
  (x:xs) -> initSol rand' xs (insertLoc i x sol)
  where
    (i, rand') = randomR (1 :: Int, length clients) rand
    insertLoc :: Int -> Int -> Solution -> Solution
    insertLoc i loc = modifyAt i (modifyAt 0 (loc:))

-- Compute the total cost of a truck's route
truckCost' :: Float -> Int -> [Int] -> Float
truckCost' cost last [] = cost + getElem last 1 routeList
truckCost' cost last (x:xs) = truckCost' (cost + getElem last x routeList) x xs

truckCost :: [Int] -> Float
truckCost = truckCost' 0 1

-- Energy function, measuring the total travel distance for the current
-- solution.
totalCost' :: Solution -> Float -> Float
totalCost' [] acc     = acc
totalCost' (x:xs) acc = totalCost' xs (truckCost (x!!1) + acc)

totalCost :: EnergyFunction Solution
totalCost sol = totalCost' sol 0

-- Temperature function, that'll give us the current temp based on the distance
-- between the current and ending time.
curTemp :: TemperatureFunction
curTemp curr = 50
  * (exp (0.0 - (5.0 * ((fromIntegral tmax) / (fromIntegral curr)))) :: Float)

-- Motion function, that swaps two random clients of position.
swapClient :: StdGen -> Solution -> Solution
swapClient rand sol = (modifyAt n1 (modifyAt 0 (setAt n2 a)))
  (modifyAt m1 (modifyAt 0 (setAt m2 b)) sol)
  where
    a = sol !! m1 !! 0 !! m2
    b = sol !! n1 !! 0 !! n2
    (m1, rand0) = randomR (0 :: Int, length sol) rand
    (n1, rand1) = randomR (0 :: Int, length sol) rand0
    (m2, rand2) = randomR (0 :: Int, length (sol !! 0 !! m1)) rand1
    (n2, rand3) = randomR (0 :: Int, length (sol !! 0 !! n1)) rand2

{-
  Transition probability function. Calculate the probability of the result
  passing, if it is worse than the last one. Then throw a die between 0 and 1
  and check the result.
-}
calcProb :: TransitionProbabilityFunction
calcProb cCurrent cChanged curTemp = if (cCurrent < cChanged)
  then 1
  else (ceiling . exp . negate) $ (cCurrent - cChanged) / curTemp

{-
Checking:
  Revisar que sea posible resolver el problema:
  - que no haya clientes que estén a más la mitad de la autonomía más alta con la
    que cuente la flota.
  - Cualificado: que no haya envíos más grandes que la capacidad de carga más alta
    con la que cuente la flota.
-}
checkValues :: Bool
checkValues = if null $ filter
  (\a -> a > (1/2)*(foldl' max 0 (map autonomy truckList)))
  $ V.toList (getRow 1 routeList)
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
