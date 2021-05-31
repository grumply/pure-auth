{-# language DerivingStrategies, DeriveGeneric, DerivingVia #-}
module Pure.Auth.Data.Token where

import Pure.Auth.Data.Key (Key)
import Pure.Auth.Data.Username (Username)

import Pure.Data.JSON (ToJSON,FromJSON)

import GHC.Generics (Generic)

newtype Token = Token (Username,Key)
  deriving (Generic,Eq)
  deriving (ToJSON,FromJSON) via (Username,Key)
