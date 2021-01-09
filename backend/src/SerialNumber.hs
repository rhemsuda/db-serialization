{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module SerialNumber (SerialNumber, mkSerialNumber) where

import Data.Text
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Aeson.Parser
import qualified Data.Text as T
import GHC.Generics (Generic)
import TypeClasses (Representable (..), IsText (..))

newtype SerialNumber = SerialNumber Text
  deriving (Generic, Show)

instance ToJSON SerialNumber
instance FromJSON SerialNumber where
  parseJSON = withText "SerialNumber" $ \s -> do
    case mkSerialNumber s of
      Just serialNumber -> pure serialNumber
      Nothing -> fail "Invalid Serial Number"

instance IsText SerialNumber where
  getText (SerialNumber sn) = sn

mkSerialNumber :: Text -> Maybe SerialNumber
mkSerialNumber s
  | T.length s /= 8 = Nothing
  | otherwise = Just $ SerialNumber s
