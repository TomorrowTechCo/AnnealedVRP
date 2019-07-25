{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module Exec where

import           Data.Aeson
import           Data.Text
import           Data.Time                (UTCTime)
import           Data.Time.Calendar
import           GHC.Generics
import           Network.Wai.Handler.Warp
import           Servant


data User = User
  { userName             :: String,
    userAge              :: Int,
    userEmail            :: String,
    userRegistrationDate :: Day } deriving (Eq, Show, Generic)

instance ToJSON User

users :: [User]
users =
  [ User "Juan" 15 "juan@dinero.com" (fromGregorian 2002 2 1),
    User "Daniela" 22 "Dani@dinero.com" (fromGregorian 2001 10 15)]

data Client = Client
  { clientName :: String
  , clientAge  :: Int } deriving (Show, Generic)
instance ToJSON Client

data Truck = Truck
  { truckPlate :: String
  , truckAge   :: Int } deriving (Show, Generic)
instance ToJSON Truck

data SortBy = Age | Name

type UserAPI = "users" :> Get '[JSON] [User]
  :<|> "Juan" :> Get '[JSON] User
  :<|> "Daniela" :> Get '[JSON] User

{-
  The web service API of our app.
  It'll allow us to retrieve relevant info from our app, like the list of
  working vehicles, the ones that need maintenance, the list of clients, etc.
  more importantly, we'll be able to retrieve the most recent truck route
  solution. The algorithm will be re-run every time new clients are inserted.
-}
type TopLevelAPI = "Clients" :> Get '[JSON] [Client]
  :<|> "Client" :> Capture "name" String :> Get '[JSON] Client
  :<|> "Trucks" :> Get '[JSON] [Truck]
  :<|> "Truck" :> Capture "plate" String :> Get '[JSON] Truck

server1 :: Server UserAPI
server1 = return users
  :<|> return (users !! 0)
  :<|> return (users !! 1)

userAPI :: Proxy UserAPI
userAPI = Proxy

app' :: Application
app' = serve userAPI server1

app :: IO ()
app = run 8081 app'
