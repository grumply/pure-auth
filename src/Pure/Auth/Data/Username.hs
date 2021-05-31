{-# language DerivingStrategies, DeriveGeneric, DerivingVia #-}
module Pure.Auth.Data.Username where

import Pure.Data.Txt (Txt,ToTxt,FromTxt)
import Pure.Data.JSON (ToJSON,FromJSON)

import Data.Hashable (Hashable)
import GHC.Generics (Generic)

newtype Username = Username Txt
  deriving stock (Generic,Show,Eq,Ord)
  deriving (Hashable,ToJSON,FromJSON,ToTxt,FromTxt) via Txt

