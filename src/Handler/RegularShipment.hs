{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Handler.RegularShipment where


import           Import

getRShipment :: (MonadError ServantErr m, MonadIO m)
        => SqlBackend -> Key RegularShipment -> m RegularShipment
getRShipment conn usrKey = do
  user :: [Entity RegularShipment]
       <- liftIO $ (flip runSqlConn) conn $
          select $ from $ \usr -> do
            where_ (usr ^. RegularShipmentId ==. val usrKey)
            return usr
  if null user
  then throwError $ err404 {errBody = "Shipment does not exist"}
  else return $! entityVal $ (user !! 0)

getRShipments :: (MonadIO m) => SqlBackend -> m [RegularShipment]
getRShipments conn = do
  users :: [Entity RegularShipment]
        <- liftIO $ (flip runSqlConn) conn $
           select $ from $ \usrs -> do
             return usrs
  return $! entityVal <$> users

postRShipment :: (MonadIO m)
             => SqlBackend -> RegularShipment -> m RegularShipmentId
postRShipment conn user = do
  userId <- liftIO $ (flip runSqlConn) conn $
    insert user
  return userId
