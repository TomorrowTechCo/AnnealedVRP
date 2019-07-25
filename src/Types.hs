{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Types where

import           Control.Monad.Reader
import           Data.Matrix
import           Data.Sequence
import           Data.Text
import           Data.Time
import qualified Data.Vector          as V
import           Database.Persist.TH
import           System.Random

{-
  Persistent data types for the app.
-}
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Client json
  name Text
  pkgWeight Double
  deriving Show
ServiceStation json
  name Text
  price Double
  deriving Show
Depot json
  name Text
  deriving Show
Truck json
  plate Text
  capacity Double
  autonomy Double
  deriving Show
|]

-- |A shipment. Could be outgoing or incoming.
data Shipment = Shipment { id :: String
  , weight                    :: Float
  , timeWindow                :: (LocalTime, LocalTime) }

-- RutaCamion es una secuencia de clientes, que empieza y termina en la sede.
-- type TruckRoute = V.Vector (Id Client)

-- Una solucion es una lista de jornadas de camiones que cumpla las restricciones.
-- Las jornadas son a su vez listas de rutas, que siempre están por debajo de la
-- autonomía.
type Solution = Seq (Seq (Id Client))

type Cost = Double
type Temperature = Double
type Time = Int
type Probability = Int
type Distance = Double

-- temporary hack
type family Id a :: *
type instance Id Client = Int

-- Funciones que se usan para el recocido simulado
type EnergyFunction a = a -> Cost
type TemperatureFunction = Time -> Temperature
type TransitionProbabilityFunction = Cost -> Cost -> Temperature -> Probability
type MotionFunction a = StdGen -> a -> (StdGen, a)

-- Config type for the reader environment.
data Config = Config
  { getTrucks  :: V.Vector Truck
  , getRoutes  :: Matrix Distance
  , getClients :: V.Vector (Id Client)
  , getTMax    :: Time }

-- Wrapping the config in a reader stack.
newtype ConfigM a = ConfigM
  { runConfigM :: ReaderT Config IO a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)

-- this type represents a possible change in the solution.
data Modif = MoveTwo (Id Client) (Int, Int) (Id Client) (Int, Int)
           | MoveOne (Id Client) (Int, Int) (Int, Int)
