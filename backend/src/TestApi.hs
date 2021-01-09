{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module TestApi (app) where

--import Control.Monad.Except
--import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types
--import Data.Attoparsec.ByteString
--import Data.ByteString (ByteString)
import Data.List
--import Data.Maybe
--import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
--import Lucid
-- import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
--import System.Directory
--import Text.Blaze
--import Text.Blaze.Html.Renderer.Utf8
--import Servant.Types.SourceT (source)
import qualified Data.Aeson.Parser
--import qualified Text.Blaze.Html

type API = "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
         :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
         :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email

data Position = Position
  { xCoord :: Int
  , yCoord :: Int
  } deriving Generic

instance ToJSON Position

newtype HelloMessage = HelloMessage { msg :: String }
  deriving Generic

instance ToJSON HelloMessage

data ClientInfo = ClientInfo
  { clientName :: String
  , clientEmail :: String
  , clientAge :: Int
  , clientInterestedIn :: [String]
  } deriving Generic

instance FromJSON ClientInfo
instance ToJSON ClientInfo

data Email = Email
  { from :: String
  , to :: String
  , subject :: String
  , body :: String
  } deriving Generic

instance ToJSON Email

emailForClient :: ClientInfo -> Email
emailForClient c = Email from' to' subject' body'
  where from' = "great@company"
        to' = clientEmail c
        subject' = "Hey " ++ clientName c ++ ", we miss you!"
        body' = "Hi " ++ clientName c ++ ",\n\n"
                ++ "Since you've recently turned " ++ show (clientAge c)
                ++ ", have you checked out our latest "
                ++ intercalate ", " (clientInterestedIn c)
                ++ " products? Give us a visit!"

server1 :: Server API
server1 = position
  :<|> hello
  :<|> marketing
  where position :: Int -> Int -> Handler Position
        position x y = return (Position x y)

        hello :: Maybe String -> Handler HelloMessage
        hello mname = return . HelloMessage $ case mname of
          Nothing -> "Hello, anonymous coward"
          Just n -> "Hello, " ++ n

        marketing :: ClientInfo -> Handler Email
        marketing clientinfo = return (emailForClient clientinfo)

proxy :: Proxy API
proxy = Proxy

app :: Application
app = serve proxy server1

-- curl -X POST -d '{"clientName": "Kyle Jensen", "clientEmail": "kyle.jensen72@gmail.com", "clientAge": 23, "clientInterestedIn": ["haskell", "snowboarding"]}' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8081/marketing
