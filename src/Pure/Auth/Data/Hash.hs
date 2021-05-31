{-# language KindSignatures, DataKinds, RoleAnnotations, DerivingStrategies, DeriveGeneric, DerivingVia #-}
module Pure.Auth.Data.Hash where

import Pure.Data.Txt (Txt)
import Pure.Data.JSON (ToJSON,FromJSON)

import GHC.Generics (Generic)

import GHC.TypeLits (Nat)

newtype Hash (rounds :: Nat) hashOf = Hash Txt
  deriving (Generic,Show,Eq,Ord)
  deriving (ToJSON,FromJSON) via Txt

type role Hash nominal nominal