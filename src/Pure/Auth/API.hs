{-# language TemplateHaskell, DerivingStrategies, DeriveGeneric, DeriveAnyClass, DuplicateRecordFields, TypeFamilies #-}
module Pure.Auth.API where

import Pure.Data.JSON (ToJSON,FromJSON)
import qualified Pure.WebSocket as WS
import Pure.WebSocket (mkMessage,mkRequest,(<:>))

import Pure.Auth.Data.Email
import Pure.Auth.Data.Key
import Pure.Auth.Data.Password
import Pure.Auth.Data.Token
import Pure.Auth.Data.Username

import GHC.Generics

--------------------------------------------------------------------------------
-- Login

data LoginRequest = LoginRequest
  { username :: Username
  , password :: Password
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

mkRequest "Login"
  [t|LoginRequest -> Maybe Token|]

--------------------------------------------------------------------------------
-- Activate

data ActivateRequest = ActivateRequest
  { username :: Username
  , key      :: Key
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

mkRequest "Activate"
  [t|ActivateRequest -> Maybe Token|]

--------------------------------------------------------------------------------
-- Verify

data VerifyRequest = VerifyRequest
  { token :: Token 
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

mkRequest "Verify"
  [t|VerifyRequest -> Bool|]

--------------------------------------------------------------------------------
-- Register

data RegisterMessage = RegisterMessage
  { username :: Username
  , email    :: Email
  , password :: Password
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)
    
mkMessage "Register"
  [t|RegisterMessage|]

--------------------------------------------------------------------------------
-- Initiate Recovery

data InitiateRecoveryMessage = InitiateRecoveryMessage
  { username :: Username
  , email    :: Email 
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

mkMessage "InitiateRecovery"
  [t|InitiateRecoveryMessage|] 

--------------------------------------------------------------------------------
-- Recover

data RecoverRequest = RecoverRequest
  { username :: Username
  , password :: Password
  , key      :: Key
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

mkRequest "Recover"
  [t|RecoverRequest -> Maybe Token|]

--------------------------------------------------------------------------------
-- Update Email

data UpdateEmailMessage = UpdateEmailMessage
  { username :: Username
  , email    :: Email
  , password :: Password
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

mkMessage "UpdateEmail"
  [t|UpdateEmailMessage|]

--------------------------------------------------------------------------------
-- Update Password

data UpdatePasswordRequest = UpdatePasswordRequest
  { username    :: Username
  , oldPassword :: Password
  , newPassword :: Password
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

mkRequest "UpdatePassword"
  [t|UpdatePasswordRequest -> Maybe Token|]

--------------------------------------------------------------------------------
-- Logout

data LogoutMessage = LogoutMessage
  { token :: Token
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

mkMessage "Logout"
  [t|LogoutMessage|]

--------------------------------------------------------------------------------
-- API

api = WS.api msgs reqs
  where
    msgs = register
       <:> initiateRecovery
       <:> updateEmail
       <:> logout
       <:> WS.none

    reqs = login
       <:> activate
       <:> verify
       <:> updatePassword
       <:> recover
       <:> WS.none
