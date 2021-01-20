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

type BackendRoutes = "api" :> "vrd" :> "create" :> ReqBody '[JSON] SerialNumber :> Post '[JSON] Text
  :<|> "api" :> "shop" :> "create" :> ReqBody '[JSON] Shop :> Post '[JSON] Text

type FrontendRoutes = "landing" :> 

type API = BackendRoutes :<|> FrontendRoutes

server :: Server API
server = createVRD
  :<|> createShop
  :<|> render
  
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

        render :: Route -> Handler Text

proxy :: Proxy API
proxy = Proxy

app :: Application
app = serve proxy server
