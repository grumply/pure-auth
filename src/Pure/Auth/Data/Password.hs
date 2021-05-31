{-# language DerivingStrategies, DeriveGeneric, DerivingVia #-}
module Pure.Auth.Data.Password where

import Pure.Data.Txt (Txt(),ToTxt,FromTxt)
import Pure.Data.JSON (ToJSON,FromJSON)

import GHC.Generics (Generic)

newtype Password = Password Txt
  deriving stock (Generic,Eq)
  deriving (ToJSON,FromJSON,ToTxt,FromTxt) via Txt
