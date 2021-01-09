{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstrainedClassMethods #-}

module TypeClasses where

import Data.Text

class Representable a where
  type Rep a
  toRep :: a -> Rep a
  fromRep :: (Eq (Rep a), Bounded a, Enum a, Representable a) => Rep a -> Maybe a
  fromRep ra = lookup ra repMap
    where repMap = fmap (\x -> (toRep x, x)) [minBound .. maxBound]

class IsText a where
  getText :: a -> Text
