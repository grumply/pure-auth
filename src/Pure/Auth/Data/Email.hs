{-# language DerivingStrategies, DeriveGeneric, DerivingVia #-}
module Pure.Auth.Data.Email where

import Pure.Data.Txt (Txt,ToTxt,FromTxt)
import Pure.Data.JSON (ToJSON,FromJSON)

import GHC.Generics (Generic)

newtype Email = Email Txt
  deriving stock (Generic,Show,Eq,Ord)
  deriving (ToJSON,FromJSON,ToTxt,FromTxt) via Txt

