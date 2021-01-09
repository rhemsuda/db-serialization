{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
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

import qualified Data.Text as T

import Shop
import SerialNumber
import TypeClasses (Representable (..), IsText (..))

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
VRDRegistry
    serialNumber String
    UVRDRegistry serialNumber
    deriving Show
ShopRegistry
    shopNumber String
    shopLocationCode String
    shopName String
    UShopRegistry shopNumber shopLocationCode
    deriving Show
VRDSubscription
    shopId ShopRegistryId
    vrdId VRDRegistryId
    subscriptionStart String
    subscriptionEnd String
    UVRDSubscription shopId vrdId
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

insertVRD :: SerialNumber -> SqlPersistT IO (Maybe InsertError)
insertVRD sn = do
  res <- insertUnique $ VRDRegistry $ T.unpack $ getText sn
  pure $ case res of
    Just _ -> Nothing
    Nothing -> Just $ RecordAlreadyStored $ show sn

insertShop :: Shop -> SqlPersistT IO (Maybe InsertError)
insertShop (Shop num loc name) = do
  res <- insertUnique $ ShopRegistry
         (T.unpack $ getText num)
         (T.unpack $ getText loc)
         (T.unpack $ getText name)
  pure $ case res of
    Just _ -> Nothing
    Nothing -> Just $ RecordAlreadyStored $ show loc ++ " " ++ show num

data RegistrationError
  = InvalidStoreNumber ShopNumber
  | AlreadyRegistered SerialNumber
