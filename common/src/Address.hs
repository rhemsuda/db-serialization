module Address where

import Data.Maybe
import Data.CountryCode

data Address = Address
  { addressCity :: Text
  , addressCountry :: CountryCode
  , addressLine1 :: Text
  , addressLine2 :: Maybe Text
  , addressPostalCode :: Text
  , addressState :: Text
  }
