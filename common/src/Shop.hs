{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}

module Shop where

import Data.Maybe
import Data.Text
import Data.Char
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics (Generic)
import qualified Data.Aeson.Parser
import qualified Data.Text as T

import TypeClasses (Representable (..), IsText (..))

data ShopError
  = ShopNumberWrongLength
  | ShopNumberNotNumeric Text
  | LocationCodeWrongLength
  | InvalidLocationCode Text
  | ShopNameExceedsMaxLength Int

instance Show ShopError where
  show ShopNumberWrongLength = "The store number must be exactly four numeric digits in length."
  show (ShopNumberNotNumeric num) = "The store number (" ++ T.unpack num ++ ") must consist of only numeric digits."
  show LocationCodeWrongLength = "The location code must be exactly two characters in length."
  show (InvalidLocationCode code) = "The location code you provided (" ++ T.unpack code ++ ") does not exist."
  show (ShopNameExceedsMaxLength len) = "The store name must be less than 32 characters. The name entered was " ++ show len ++ " characters long."


data Shop = Shop
  { shopNumber :: ShopNumber
  , shopLocationCode :: LocationCode
  , shopName :: ShopName
  } deriving Generic

instance Eq Shop where
  (==) (Shop n1 l1 _) (Shop n2 l2 _) =
    n1 == n2 && l1 == l2

instance ToJSON Shop
instance FromJSON Shop where
  parseJSON = withObject "Shop" $ \o -> do
    res <- mkShop
      <$> o .: "shopNumber"
      <*> o .: "shopLocationCode"
      <*> o .: "shopName"
    case res of
      Right shop -> pure shop
      Left e -> fail $ show e


newtype ShopNumber =
  ShopNumber Text
  deriving (ToJSON, FromJSON, Generic, Eq, Show)

instance IsText ShopNumber where
  getText (ShopNumber sn) = sn

  
data LocationCode =
  NL | PE | NS | NB | QC | ON | MB | SK | AB | BC | YT | NT | NU
  deriving (ToJSON, FromJSON, Generic, Eq, Show, Bounded, Enum)

instance IsText LocationCode where
  getText NL = "NL"
  getText PE = "PE"
  getText NS = "NS"
  getText NB = "NB"
  getText QC = "QC"
  getText ON = "ON"
  getText MB = "MB"
  getText SK = "SK"
  getText AB = "AB"
  getText BC = "BC"
  getText YT = "YT"
  getText NT = "NT"
  getText NU = "NU"  

instance Representable LocationCode where
  type Rep LocationCode = Text  
  toRep lc = getText lc


newtype ShopName =
  ShopName Text
  deriving (ToJSON, FromJSON, Generic, Eq, Show)

instance IsText ShopName where
  getText (ShopName sn) = sn


mkShopNumber :: Text -> Either ShopError ShopNumber
mkShopNumber num
  | T.length num /= 4 = Left $ ShopNumberWrongLength
  | elem False $ fmap isNumber (T.unpack num) = Left $ ShopNumberNotNumeric num
  | otherwise = Right $ ShopNumber num

mkLocationCode :: Text -> Either ShopError LocationCode
mkLocationCode lc
  | T.length lc /= 2 = Left $ LocationCodeWrongLength
  | otherwise = maybe (Left $ InvalidLocationCode lc) Right (fromRep lc)

mkShopName :: Text -> Either ShopError ShopName
mkShopName name
  | T.length name > 32 = Left $ ShopNameExceedsMaxLength $ T.length name
  | otherwise = Right $ ShopName name

mkShop :: Text -> Text -> Text -> (Either ShopError Shop)
mkShop num loc name = Shop <$> mkShopNumber num <*> mkLocationCode loc <*> mkShopName name

