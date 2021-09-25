{-# language LambdaCase, TypeApplications, RecordWildCards, NamedFieldPuns, RankNTypes, DeriveAnyClass, OverloadedStrings, DuplicateRecordFields, TypeFamilies, FlexibleContexts #-}
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

data Recover = Recover 
  { socket    :: WebSocket
  , onSuccess :: IO ()
  , onLogin   :: IO ()
  } deriving Theme

instance Component Recover where
  data Model Recover = Model
    { invalid  :: Bool
    , username :: Username
    , email    :: Email
    }

  model = Model {..}
    where
      invalid  = False
      username = fromTxt ""
      email    = fromTxt ""

  data Msg Recover
    = SetUsername Username 
    | SetEmail Email 
    | Submit

  upon = \case
    SetUsername un -> setUsername un
    SetEmail e     -> setEmail e
    Submit         -> submit

  view Recover { onLogin } Model {..} = let status | invalid = Themed @Invalid | otherwise = id in
    Div <| Themed @Recover . status |>
      [ Input <| OnInput (withInput (command . SetUsername . fromTxt)) . Placeholder "Username" . Type "name"
      , Input <| OnInput (withInput (command . SetEmail . fromTxt)) . Placeholder "Email" . Type "email"
      , Button <| OnClick (const (command Submit)) |> 
        [ "Recover" ]
      , Button <| OnClick (const onLogin) |> 
        [ "Log In" ]
      ]

setUsername :: Username -> Update Recover
setUsername un _ mdl = pure mdl { username = un }

setEmail :: Email -> Update Recover
setEmail e _ mdl = pure mdl { email = e }

submit :: Update Recover
submit Recover { socket, onSuccess } mdl@Model { username, email } = do
  message Auth.api socket Auth.initiateRecovery Auth.InitiateRecoveryMessage {..}
  onSuccess
  pure mdl

data Invalid deriving Theme