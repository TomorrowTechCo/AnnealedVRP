{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
module Types where

import           Control.Monad.Reader
import           Data.Matrix
import           Data.Text
import           Data.Time
import qualified Data.Vector          as V
import           System.Random

-- |A location in the map. Could be a client, a service station, or a depot.
-- data Location =
--   Client
--   { name        :: Text
--   , pkgWeight   :: Float
--   , arrivalTime :: LocalTime }
--   | ServiceStation
--   { name  :: Text
--   , price :: Float }
--   | Depot
--   { name :: Text }

-- as individual data types with typeclasses
data Client = Client
  { clientId          :: Int
  , clientName        :: Text
  , clientPkgWeight   :: Float
  , clientArrivalTime :: LocalTime } deriving Show
data SStation = SStation
  { serviceStationId    :: Int
  , serviceStationName  :: Text
  , serviceStationPrice :: Float } deriving Show
data Depot = Depot
  { depotId   :: Int
  , depotName :: Text } deriving Show

class Location k where
  type Id k :: *
  getName :: k -> Text
  getId   :: k -> Int

instance Location Client where
  type Id Client = Int
  getName = clientName
  getId = clientId

instance Location SStation where
  type Id SStation = Int
  getName = serviceStationName
  getId = serviceStationId

instance Location Depot where
  type Id Depot = Int
  getName = depotName
  getId = depotId

-- |A shipment. Could be outgoing or incoming.
data Shipment = Shipment { id :: String
  , weight                    :: Float
  , timeWindow                :: (LocalTime, LocalTime) }

-- Camion tiene una autonomia A, y capacidad de carga C
data Truck = Truck
  { plate    :: String
  , capacity :: Float
  , autonomy :: Float } deriving Show

-- RutaCamion es una secuencia de clientes, que empieza y termina en la sede.
type TruckRoute = [Id Client]

-- Una solucion es una lista de jornadas de camiones que cumpla las restricciones.
-- Las jornadas son a su vez listas de rutas, que siempre están por debajo de la
-- autonomía.
type Solution = [[TruckRoute]]

type Cost = Float
type Temperature = Float
type Time = Int
type Probability = Int
type Distance = Float

-- Funciones que se usan para el recocido simulado
type EnergyFunction a = a -> Cost
type TemperatureFunction = Time -> Temperature
type TransitionProbabilityFunction = Cost -> Cost -> Temperature -> Probability
type MotionFunction a = StdGen -> a -> (StdGen, a)

-- Config type for the reader environment.
data Config = Config
  { getTrucks  :: V.Vector Truck
  , getRoutes  :: Matrix Distance
  , getClients :: V.Vector Client
  , getTMax    :: Time }

-- Wrapping the config in a reader stack.
newtype ConfigM a = ConfigM
  { runConfigM :: ReaderT Config IO a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)

-- this type represents a possible change in the solution.
data Modif = MoveTwo (Id Client) (Int, Int) (Id Client) (Int, Int)
           | MoveOne (Id Client) (Int, Int) (Int, Int)

-- monad for quickly accessing values from matrix
data MatrixM a = MatrixM (Matrix a) a [a]
-- meaning: a MatrixM of type Client Id

-- instance Functor MatrixM where
--   fmap f ()

{-
  monad for accessing random values in different ranges
  expected interface
  someFunc a b = a + m1 + b + m2
    where
      [m1, m2] = do
        getRnd (0, 5)
        getRnd (1, 4)

  we pass the config to the monad through a "run" function
  runRand :: RandomM a -> StdGen -> (StdGen, [a])

  someFunc a b rand = a + m1 + b + m2
    where
      (rand', [m1, m2]) = runRand rand $ do
        ...etc

  RandomM Int returns a list of ints.
  the constructor is as follows:
  newtype RandomM a = RandomM (StdGen, [a])

  the monad carries around the list of generated values and the
  random generator.

  fmap :: (a -> b) -> RandomM a -> RandomM b
  (<*>) :: RandomM (a -> b) -> RandomM a -> RandomM b
  (>>=) :: (a -> RandomM b) -> RandomM a -> RandomM b

-}
