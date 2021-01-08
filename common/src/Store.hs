{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Store where

import Data.String
import Data.Char
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Aeson.Parser
import qualified Data.Text as T
import GHC.Generics

data Store = Store
  { storeNumber :: StoreNumber
  , storeLocationCode :: LocationCode
  , storeName :: StoreName
  } deriving Generic

instance Eq Store where
  (==) (Store n1 l1 _) (Store n2 l2 _) = n1 == n2 && l1 == l2

instance FromJSON Store where
  parseJSON = withObject "Store" $ \o -> do
    res <- mkStore <$> o .: "storeNumber" <*> o .: "storeLocationCode" <*> o .: "storeName"
    case res of
      Right store -> pure store
      Left e -> fail $ show e
                                         
instance ToJSON Store

mkStore :: String -> String -> String -> Either StoreError Store
mkStore num loc name = Store <$> mkStoreNumber num <*> mkLocationCode loc <*> mkStoreName name

data StoreError
  = StoreNumberWrongLength
  | StoreNumberNotNumeric String
  | LocationCodeWrongLength
  | InvalidLocationCode String [LocationCode]
  | StoreNameExceedsMaxLength Int

instance Show StoreError where
  show StoreNumberWrongLength = "The store number must be exactly four numeric digits in length."
  show (StoreNumberNotNumeric num) = "The store number (" ++ num ++ ") must consist of only numeric digits."
  show LocationCodeWrongLength = "The location code must be exactly two characters in length."
  show (InvalidLocationCode code validCodes) = "The location code you provided (" ++ code ++ ") does not exist. Please choose from one of the following: " ++ show validCodes ++ "."
  show (StoreNameExceedsMaxLength len) = "The store name must be less than 32 characters. The name entered was " ++ show len ++ " characters long."


newtype StoreNumber =
  StoreNumber String
  deriving (ToJSON, FromJSON, Generic, Eq)

instance Show StoreNumber where
  show (StoreNumber s) = s

mkStoreNumber :: String -> Either StoreError StoreNumber
mkStoreNumber num
  | length num /= 4 = Left $ StoreNumberWrongLength
  | elem False $ fmap isNumber num = Left $ StoreNumberNotNumeric num
  | otherwise = Right $ StoreNumber num
  

data LocationCode =
  NL | PE | NS | NB | QC | ON | MB | SK | AB | BC | TY | NT | NU
  deriving (ToJSON, FromJSON, Generic, Eq, Show)

getCode :: LocationCode -> T.Text
getCode l = T.pack (show l)

  -- deriving (ToJSON, FromJSON, Generic, Eq)

-- instance Show LocationCode where
--   show (LocationCode s) = s

-- supportedLocationCodes :: [LocationCode]
-- supportedLocationCodes =
--   [ LocationCode "NL"
--   , LocationCode "PE"
--   , LocationCode "NS"
--   , LocationCode "NB"
--   , LocationCode "QC"
--   , LocationCode "ON"
--   , LocationCode "MB"
--   , LocationCode "SK"
--   , LocationCode "AB"
--   , LocationCode "BC"
--   , LocationCode "YT"
--   , LocationCode "NT"
--   , LocationCode "NU"
--   ]

-- locToStr :: [LocationCode] -> [String]
-- locToStr = map show

mkLocationCode :: String -> Either StoreError LocationCode
mkLocationCode code
  | length code /= 2 = Left $ LocationCodeWrongLength
  | otherwise = Right $ ON

  -- | length code /= 2 = Left $ LocationCodeWrongLength
  -- | elem code $ locToStr supportedLocationCodes = Right $ LocationCode code
  -- | otherwise = Left $ InvalidLocationCode code supportedLocationCodes

newtype StoreName =

  StoreName String
  deriving (ToJSON, FromJSON, Generic, Eq)

instance Show StoreName where
  show (StoreName s) = s
  
mkStoreName :: String -> Either StoreError StoreName
mkStoreName name
  | length name > 32 = Left $ StoreNameExceedsMaxLength $ length name
  | otherwise = Right $ StoreName name
