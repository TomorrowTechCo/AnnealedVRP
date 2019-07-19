{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Types where

import           Data.Matrix
import           Data.Text
import           Data.Time
import qualified Data.Vector.Unboxed as V
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
  { clientId          :: Integer
  , clientName        :: Text
  , clientPkgWeight   :: Float
  , clientArrivalTime :: LocalTime } deriving Show
data SStation = SStation
  { serviceStationId    :: Integer
  , serviceStationName  :: Text
  , serviceStationPrice :: Float } deriving Show
data Depot = Depot
  { depotId   :: Integer
  , depotName :: Text } deriving Show

class Location k where
  type Id k :: *
  getName :: k -> Text
  getId   :: k -> Integer

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

-- Funciones que se usan para el recocido simulado
type EnergyFunction a = a -> Cost
type TemperatureFunction = Time -> Temperature
type TransitionProbabilityFunction = Cost -> Cost -> Temperature -> Probability
type MotionFunction a = StdGen -> a -> (StdGen, a)
