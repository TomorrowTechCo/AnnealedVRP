{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Account where

import           Import

getAccount :: (MonadIO m, MonadError ServantErr m)
          => SqlBackend -> Key Account -> m Account
getAccount conn nit = do
  account :: [Entity Account]
         <- liftIO $ (flip runSqlConn) conn $
            select $ from $ \clt -> do
              where_ (clt ^. AccountId ==. val nit)
              return clt
  if null account
  then throwError $ err404 { errBody = "Account does not exist" }
  else return $! entityVal $ (account !! 0)

getAccounts :: MonadIO m => SqlBackend -> m [Account]
getAccounts conn = do
  accounts :: [Entity Account]
          <- liftIO $ (flip runSqlConn) conn $
             select $ from $ \clt -> do
               return clt
  return $! entityVal <$> accounts

postAccount :: MonadIO m => SqlBackend -> Account -> m AccountId
postAccount conn account = do
  accountId <- liftIO $ (flip runSqlConn) conn $
    insert account
  return $! accountId
