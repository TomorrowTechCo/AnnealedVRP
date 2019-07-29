{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Exec where

import           Control.Monad.Except        (MonadError)
import           Control.Monad.IO.Class
import           Control.Monad.IO.Unlift
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
import           Database.Persist            (selectFirst, selectList)
import           Database.Persist.Postgresql hiding ((==.))
import           Database.Persist.Types      (entityVal)

import           Types

connStr :: ConnectionString
connStr = "host=localhost dbname=testdb user=testuser password=test port=5432"

-- app :: IO ()
-- app = do
--   dbPool :: Pool SqlBackend
--          <- runStdoutLoggingT $ createPostgresqlPool connStr 20
--   putStrLn "this ain't over yet"

{-
  The web service API of our app.
  It'll allow us to retrieve relevant info from our app, like the list of
  working vehicles, the ones that need maintenance, the list of clients, etc.
  more importantly, we'll be able to retrieve the most recent truck route
  solution. The algorithm will be re-run every time new clients are inserted.
-}
type TopLevelAPI =
       "Clients" :> Capture "name" String :> Get '[JSON] Client
  :<|> "Clients" :> ReqBody '[JSON] Client :> Post '[JSON] ClientId
  :<|> "Clients" :> Get '[JSON] [Client]
  :<|> "Trucks" :> Capture "plate" String :> Get '[JSON] Truck
  :<|> "Trucks" :> ReqBody '[JSON] Truck :> Post '[JSON] TruckId
  :<|> "Trucks" :> Get '[JSON] [Truck]

topLevelAPI :: Proxy TopLevelAPI
topLevelAPI = Proxy

getClient :: (MonadIO m, MonadError ServantErr m)
          => SqlBackend -> String -> m Client
getClient conn name = do
  client :: [Entity Client]
         <- liftIO $ (flip runSqlConn) conn $
            select $ from $ \clt -> do
              where_ (clt ^. ClientName ==. (val $ pack name))
              return clt
  if null client
  then throwError $ err404 { errBody = "User does not exist" }
  else return $! entityVal $ (client !! 0)

getClients :: MonadIO m =>  m [Client]
getClients = liftIO . return $ [Client "Juan" 123, Client "diana" 456]

postClient :: MonadIO m =>  Client -> m ClientId
postClient = error "sorry pal"
getTruck :: MonadIO m =>  String -> m Truck
getTruck = error "not yet implemented"
getTrucks :: MonadIO m =>  m [Truck]
getTrucks = error "does not work yet"
postTruck :: MonadIO m =>  Truck -> m TruckId
postTruck = error "sorry pal"

app' :: Pool SqlBackend -> Application
app' pool = serve topLevelAPI $ withRes pool getClient
  :<|> postClient
  :<|> getClients
  :<|> getTruck
  :<|> postTruck
  :<|> getTrucks
  where
    withRes :: (MonadBaseControl IO m) => Pool a -> (a -> b -> m c) -> b -> m c
    withRes pool f b = withResource pool $ \a -> f a b
    -- TODO: push this inside of a reader monad

migrateTables :: IO ()
migrateTables = do
  runStdoutLoggingT $ withPostgresqlConn connStr $ \conn ->
    liftIO $ do
      flip runSqlConn conn $ do
        runMigration migrateAll
  putStrLn "migration completed"

app :: IO ()
app = do
  migrateTables
  dbPool <- runStdoutLoggingT $ createPostgresqlPool connStr 20
  run 8082 $ app' dbPool
