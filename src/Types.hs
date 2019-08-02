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
import qualified Data.Vector            as V
import           Database.Persist.Quasi
import           Database.Persist.TH
import           System.Random

{-
  These datatypes represent data to be stored in the database. They are
  consequently defined in a separate file. The remainder of this file concerns
  itself with the algorithm data types & structures.
  Account: represents a company account on the service.
  User: represents an individual user, employee of some account.
  Vehicle: each entry here is a vehicle, owned by an account.
  Location: locations of pick up and drop off of shipments.
  singular shipment: a single trip, done by a vehicle, to move a product from
  one location to another.
  regular shipment: logs an arrangement for regular deliveries at specified
  locations.
-}
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
  $(persistFileWith lowerCaseSettings "config/models")

-- -- type synonyms employed by the algorithm.
-- This represents the distance between two locations, in terms of estimated
-- cost. The algorithm receives a matrix filled with these values, with each
-- square representing the cost of traveling from row's location to the
-- column's location. Thus, this matrix is a complete graph of relative
-- distances.
type Distance = Double
-- An index represents the position of a location in the matrix. This value is
-- independent of Ids or other such identifying values in the database for
-- locations.
type Index = Int
-- cost associated to each solution.
type Cost = Double
-- global value employed by the annealing algorithm to compute odds of accepting
-- worse solutions.
type Temperature = Double
-- number of generations left for the algorithm.
type Time = Int
-- number between 0 and 100 representing the probability of acceptance.
type Probability = Int

-- A solution is represented by a list of each individual vehicle's journey.
-- internally, this list is implemented by a haskell sequence, to ease random
-- access. Each journey is, at the same time, a haskell sequence of indexes,
-- representing each location in the order they appear in the matrix of
-- distances.
type Solution = Seq (Seq Index)

-- Funciones que se usan para el recocido simulado
type EnergyFunction a = a -> Cost
type TemperatureFunction = Time -> Temperature
type TransitionProbabilityFunction = Cost -> Cost -> Temperature -> Probability
type MotionFunction a = StdGen -> a -> (StdGen, a)

-- Config type for the reader environment.
data Config = Config
  { forTrucks  :: V.Vector Vehicle
  , forRoutes  :: Matrix Distance
  , forClients :: V.Vector Index
  , forTMax    :: Time }

-- this type represents a possible change in the solution.
data Modif = MoveTwo Index (Int, Int) Index (Int, Int)
           | MoveOne Index (Int, Int) (Int, Int)

-- This config type holds
