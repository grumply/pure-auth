{-# language AllowAmbiguousTypes, TypeApplications, ScopedTypeVariables #-}
module Pure.Auth.GHC (module Export,authDB) where

import Pure.Auth.GHC.API as Export
import Pure.Auth.GHC.Auth as Export (AuthEvent(Deleted),Stream(AuthEventStream),Auth)

import Pure.Sorcerer (Listener,listener)

import Data.Typeable

authDB :: forall _role. Typeable _role => [Listener]
authDB = [listener @(AuthEvent _role) @(Auth _role) ]
