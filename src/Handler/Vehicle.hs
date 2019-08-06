{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Vehicle where

import           Import


getVehicle :: (MonadIO m, MonadError ServantErr m)
           => SqlBackend -> VehicleId -> m Vehicle
getVehicle conn plate = do
  vehicle :: [Entity Vehicle]
           <- liftIO $ (flip runSqlConn) conn $
              select $ from $ \vcl -> do
                where_ (vcl ^. VehicleId ==. val plate)
                return vcl
  if null vehicle
  then throwError $ err404 { errBody = "User does not exist" }
  else return $! entityVal $ (vehicle !! 0)

getVehicles :: MonadIO m => SqlBackend -> m [Vehicle]
getVehicles conn = do
  vehicles :: [Entity Vehicle] <- liftIO $ (flip runSqlConn) conn $
    select $ from $ \vcl -> do
      return vcl
  return $! entityVal <$> vehicles

postVehicle :: MonadIO m => SqlBackend -> Vehicle -> m VehicleId
postVehicle conn vehicle = do
  vehicleId <- liftIO $ (flip runSqlConn) conn $
    insert vehicle
  return $! vehicleId

