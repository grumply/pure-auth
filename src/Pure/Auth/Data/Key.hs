{-# language DerivingStrategies, DeriveGeneric, DerivingVia #-}
module Pure.Auth.Data.Key where

import Pure.Data.JSON (ToJSON,FromJSON)
import Pure.Data.Txt as Txt

import GHC.Generics (Generic)

newtype Key = Key Txt
  deriving (Generic,Eq)
  deriving (ToJSON,FromJSON) via Txt