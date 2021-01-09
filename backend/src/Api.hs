{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Api (app) where

import Data.Text
import qualified Data.Text as T

import Data.List
import Data.Time.Calendar
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Database
import SerialNumber
import Shop

type ConsumerID = String
type MachineID = String
type StoreID = String

type API = "vrd" :> "create" :> ReqBody '[JSON] SerialNumber :> Post '[JSON] Text
  :<|> "shop" :> "create" :> ReqBody '[JSON] Shop :> Post '[JSON] Text

server :: Server API
server = createVRD
  :<|> createShop
  
  where createVRD :: SerialNumber -> Handler Text
        createVRD serialNumber = runDb $ do
          res <- insertVRD serialNumber
          pure $ case res of
            (Just e) -> T.pack $ show e 
            Nothing -> T.pack $ "Success"

        createShop :: Shop -> Handler Text
        createShop shop = runDb $ do
          res <- insertShop shop
          pure $ case res of
            (Just e) -> T.pack $ show e
            Nothing -> T.pack $ "Success"

proxy :: Proxy API
proxy = Proxy

app :: Application
app = serve proxy server
