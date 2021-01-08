{-# LANGUAGE DeriveGeneric #-}

module SerialNumber (
  SerialNumber
  ) where

import Data.String
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import qualified Data.Aeson.Parser
import qualified Data.Text as T

newtype SerialNumber =
  SerialNumber String
  deriving (Generic)

mkSerialNumber :: String -> Maybe SerialNumber
mkSerialNumber s
  | length s /= 8 = Nothing
  | otherwise = Just $ SerialNumber s

instance ToJSON SerialNumber
instance FromJSON SerialNumber where
  parseJSON = withText "SerialNumber" $ \s -> do
    case mkSerialNumber $ T.unpack s of
      Just serialNumber -> pure serialNumber
      Nothing -> fail "Invalid Serial Number"

instance Show SerialNumber where
  show (SerialNumber s) = s
