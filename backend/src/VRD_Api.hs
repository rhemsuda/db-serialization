{-# LANGUAGE DataKinds #-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module VRD_Api (app) where

import Data.List
import Data.Time.Calendar
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Database
import SerialNumber
import Store

type ConsumerID = String
type MachineID = String
type StoreID = String

type API = "vrd" :> "create" :> ReqBody '[JSON] SerialNumber :> Post '[JSON] String
  -- :<|> "vrd" :> "register" :> ReqBody '[JSON] VRDRegister :> Post '[JSON] (Maybe VRD)
  :<|> "store" :> "create" :> ReqBody '[JSON] Store :> Post '[JSON] String

server :: Server API
server = createVRD
  -- :<|> registerVRD
  :<|> createStore
  
  where createVRD :: SerialNumber -> Handler String -- (Either DbError DbSuccess)
        createVRD serialNumber = runDb $ do
          res <- insertVRD serialNumber
          pure $ case res of
            (Just e) -> show e 
            Nothing -> show serialNumber

        createStore :: Store -> Handler String
        createStore (Store num loc name) = runDb $ do
          res <- insertStore num loc name
          pure $ case res of
            (Just e) -> show e
            Nothing -> show loc ++ show num
            
        -- registerVRD :: VRDRegister -> Handler (Maybe VRD)
        -- registerVRD registerinfo = return $ Just (VRD "vrd-154232-132234" [] max_capacity)

proxy :: Proxy API
proxy = Proxy

app :: Application
app = serve proxy server
