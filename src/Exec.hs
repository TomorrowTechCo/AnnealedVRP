{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Exec where

import           Control.Monad.Except        (MonadError)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Reader
import           Data.Aeson
import           Data.Pool
import           Data.Text                   (pack)
import           GHC.Generics                hiding (from)
import           Network.Wai.Handler.Warp
import           Servant
import           System.Random               (newStdGen)

import           Database.Esqueleto
import           Database.Persist.Postgresql hiding ((==.))
import           Database.Persist.Types      (entityVal)

import           Functions
import           Handler.Account
import           Handler.Vehicle
import           Types



{-
  The web service API of our app.
  It'll allow us to retrieve relevant info from our app, like the list of
  working vehicles, the ones that need maintenance, the list of clients, etc.
  more importantly, we'll be able to retrieve the most recent truck route
  solution. The algorithm will be re-run every time new clients are inserted.
-}
type TopLevelAPI =
       "Accounts" :> (Capture "nit" Int :> Get '[JSON] Account
                      :<|> ReqBody '[JSON] Account :> Post '[JSON] AccountId
                      :<|> Get '[JSON] [Account])
  :<|> "Vehicles" :> (Capture "plate" String :> Get '[JSON] Vehicle
                      :<|> ReqBody '[JSON] Vehicle :> Post '[JSON] VehicleId
                      :<|> Get '[JSON] [Vehicle])

topLevelAPI :: Proxy TopLevelAPI
topLevelAPI = Proxy
-- --

app' :: Pool SqlBackend -> Application
app' pool = serve topLevelAPI $
       (withRes' pool getAccount
         :<|> withRes' pool postAccount
         :<|> withRes pool getAccounts)
  :<|> (withRes' pool getVehicle
         :<|> withRes' pool postVehicle
         :<|> withRes pool getVehicles)
  where
    -- zero args
    withRes :: (MonadBaseControl IO m) => Pool a -> (a -> m c) -> m c
    withRes pool f = withResource pool $ \a -> f a
    -- one arg
    withRes' :: (MonadBaseControl IO m) => Pool a -> (a -> b -> m c) -> b -> m c
    withRes' pool f b = withResource pool $ \a -> f a b
    -- TODO: push this inside of a reader monad

migrateTables :: IO ()
migrateTables = do
  runStdoutLoggingT $ withPostgresqlConn connStr $ \conn ->
    liftIO $ do
      flip runSqlConn conn $ do
        runMigration migrateAll
  putStrLn "migration completed"

connStr :: ConnectionString
connStr = "host=localhost dbname=testdb user=testuser password=test port=5432"

app :: IO ()
app = do
  migrateTables
  dbPool <- runStdoutLoggingT $ createPostgresqlPool connStr 20
  run 8082 $ app' dbPool
