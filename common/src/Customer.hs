module Customer where

import Data.Text
import Data.Maybe
import Data.PhoneNumber
import Data.CountryCode
import Text.Email.Validate

newtype Name = Name Text
newtype CustomerDescription = CustomerDescription Text

data Customer = Customer
  { customerName :: Name
  , customerDesc :: CustomerDescription
  , customerEmail :: EmailAddress
  , customerPhone :: PhoneNumber
  , customerAddress :: Address
  , customerShippingInfo :: ShippingInfo
  -- , customerPaymentMethod :: PaymentMethod
  }

data Address = Address
  { addressCity :: Text
  , addressCountry :: CountryCode
  , addressLine1 :: Text
  , addressLine2 :: Maybe Text
  , addressPostalCode :: Text
  , addressState :: Text
  }

data ShippingInfo = ShippingInfo
  { shippingAddress :: Address
  , shippingCustomerName :: Name
  , shippingCustomerPhone :: PhoneNumber
  }

-- data PaymentMethod = PaymentMethod
--   { 

