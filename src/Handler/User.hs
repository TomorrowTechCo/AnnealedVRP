{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Handler.User where

import           Import

getUser :: (MonadError ServantErr m, MonadIO m)
        => SqlBackend -> Key User -> m User
getUser conn usrKey = do
  user :: [Entity User]
       <- liftIO $ (flip runSqlConn) conn $
          select $ from $ \usr -> do
            where_ (usr ^. UserId ==. val usrKey)
            return usr
  if null user
  then throwError $ err404 {errBody = "User does not exist"}
  else return $! entityVal $ (user !! 0)

getUsers :: (MonadIO m) => SqlBackend -> m [User]
getUsers conn = do
  users :: [Entity User]
        <- liftIO $ (flip runSqlConn) conn $
           select $ from $ \usrs -> do
             return usrs
  return $! entityVal <$> users

postUser :: (MonadIO m) => SqlBackend -> User -> m UserId
postUser conn user = do
  userId <- liftIO $ (flip runSqlConn) conn $
    insert user
  -- users :: [Entity User] <- runDB $ select $ from $ \usrs -> do
  --   return usrs
  return userId
