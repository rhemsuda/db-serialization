{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Trans.Reader
import Control.Monad.Catch

import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH

import Store
import SerialNumber

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
VRDRegistry
    serialNumber String
    UVRDRegistry serialNumber
    deriving Show
StoreRegistry
    storeNumber String
    storeLocationCode String
    storeName String
    UStoreRegistry storeNumber storeLocationCode
    deriving Show
VRDSubscription
    storeId StoreRegistryId
    vrdId VRDRegistryId
    subscriptionStart String
    subscriptionEnd String
    UVRDSubscription storeId vrdId
    deriving Show
|]

connectionString :: ConnectionString
connectionString = "host=localhost dbname=vrd user=rhemsuda password=monkeys123 port=5432"

runDb :: (MonadIO m) => SqlPersistT IO a -> m a
runDb action = liftIO $ runStderrLoggingT $
  withPostgresqlConn connectionString $ \backend ->
  liftIO $ runReaderT action backend

migrateDb :: IO ()
migrateDb = runDb $ runMigration migrateAll

data InsertError
  = RecordAlreadyStored String

instance Show InsertError where
  show (RecordAlreadyStored s) = s ++ " has already been stored."
  
-- data SerialNumberError =
--   SerialAlreadyStored SerialNumber

-- instance Show SerialNumberError where
--   show (SerialAlreadyStored sn) = show sn ++ " has already been stored."
  
-- insertVRD :: SerialNumber -> SqlPersistT IO (Maybe (InsertError VRDRegistry)))
-- insertVRD sn = do
--   res <- try (insert $ VRDRegistry $ show sn)
--   pure $ case res of
--     Right _ -> Nothing
--     Left (_ :: SomeException) -> Just $ SerialAlreadyStored sn


insertVRD :: SerialNumber -> SqlPersistT IO (Maybe InsertError)
insertVRD sn = do
  res <- insertUnique $ VRDRegistry $ show sn
  pure $ case res of
    Just _ -> Nothing
    Nothing -> Just $ RecordAlreadyStored $ show sn

insertStore :: StoreNumber
            -> LocationCode
            -> StoreName
            -> SqlPersistT IO (Maybe InsertError)
insertStore num loc name = do
  res <- insertUnique $ StoreRegistry (show num) (show loc) (show name)
  pure $ case res of
    Just _ -> Nothing
    Nothing -> Just $ RecordAlreadyStored $ show loc ++ show num

-- data RegistrationError
--   = InvalidStoreNumber StoreNumber
--   | AlreadyRegistered SerialNumber
--   | SubscriptionInactive

-- registerVRD :: SerialNumber -> StoreNumber -> SqlPersistT IO (Either RegistrationError VRD)
-- registerVRD serialNumber storeNumber


-- getNames :: SqlPersistT IO [String]
-- getNames = do
--   p <- selectList [] []
--   return $ fmap (personName . entityVal) p    
