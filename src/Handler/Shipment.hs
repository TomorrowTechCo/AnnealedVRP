{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Shipment where

import           Import

getShipment :: (MonadError ServantErr m, MonadIO m)
            => SqlBackend -> Key SingularShipment -> m SingularShipment
getShipment conn usrKey = do
  user :: [Entity SingularShipment]
       <- liftIO $ (flip runSqlConn) conn $
          select $ from $ \usr -> do
            where_ (usr ^. SingularShipmentId ==. val usrKey)
            return usr
  if null user
  then throwError $ err404 {errBody = "Shipment does not exist"}
  else return $! entityVal $ (user !! 0)

getShipments :: (MonadIO m) => SqlBackend -> m [SingularShipment]
getShipments conn = do
  users :: [Entity SingularShipment]
        <- liftIO $ (flip runSqlConn) conn $
           select $ from $ \usrs -> do
             return usrs
  return $! entityVal <$> users

postShipment :: (MonadIO m)
             => SqlBackend -> SingularShipment -> m SingularShipmentId
postShipment conn user = do
  userId <- liftIO $ (flip runSqlConn) conn $
    insert user
  return userId
