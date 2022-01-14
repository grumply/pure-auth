{-# language LambdaCase, TypeApplications, RecordWildCards, NamedFieldPuns, RankNTypes, DeriveAnyClass, OverloadedStrings, DuplicateRecordFields, TypeFamilies, FlexibleContexts, ScopedTypeVariables #-}
module Pure.Auth.GHCJS.Access.Recover ( Recover(..) ) where

import qualified Pure.Auth.API as Auth
import Pure.Auth.Data.Email
import Pure.Auth.Data.Username
import Pure.Auth.Data.Token

import Pure.Elm.Component hiding (invalid)
import qualified Pure.Data.LocalStorage as LS
import Pure.Data.Txt
import Pure.WebSocket (WebSocket,message)

import Control.Concurrent (newEmptyMVar,putMVar,takeMVar)
import Data.Typeable

data Recover (_role :: *) = Recover 
  { socket    :: WebSocket
  , onSuccess :: IO ()
  , onLogin   :: IO ()
  } deriving Theme

instance Typeable _role => Component (Recover _role) where
  data Model (Recover _role) = Model
    { invalid  :: Bool
    , username :: Username
    , email    :: Email
    }

  model = Model {..}
    where
      invalid  = False
      username = fromTxt ""
      email    = fromTxt ""

  data Msg (Recover _role)
    = SetUsername Username 
    | SetEmail Email 
    | Submit

  upon = \case
    SetUsername un -> setUsername un
    SetEmail e     -> setEmail e
    Submit         -> submit

  view Recover { onLogin } Model {..} = let status | invalid = Themed @Invalid | otherwise = id in
    Form <| Themed @(Recover _role) . OnSubmitWith intercept def . status |>
      [ Input <| TabIndex 0 . OnInput (withInput (command . SetUsername . fromTxt)) . Placeholder "Username" . Type "name"
      , Input <| TabIndex 0 . OnInput (withInput (command . SetEmail . fromTxt)) . Placeholder "Email" . Type "email"
      , Button <| TabIndex 0 . OnClick (const (command Submit)) |> 
        [ "Recover" ]
      , Button <| TabIndex 0 . OnClick (const onLogin) |> 
        [ "Log In" ]
      ]

setUsername :: Username -> Update (Recover _role)
setUsername un _ mdl = pure mdl { username = un }

setEmail :: Email -> Update (Recover _role)
setEmail e _ mdl = pure mdl { email = e }

submit :: forall _role. Typeable _role => Update (Recover _role)
submit Recover { socket, onSuccess } mdl@Model { username, email } = do
  message (Auth.api @_role) socket (Auth.initiateRecovery @_role) Auth.InitiateRecoveryMessage {..}
  onSuccess
  pure mdl

data Invalid deriving Theme