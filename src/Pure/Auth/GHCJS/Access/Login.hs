{-# language LambdaCase, TypeApplications, RecordWildCards, NamedFieldPuns, RankNTypes, DeriveAnyClass, OverloadedStrings, DuplicateRecordFields, TypeFamilies, FlexibleContexts #-}
module Pure.Auth.GHCJS.Access.Login ( Login(..) ) where

import qualified Pure.Auth.API as Auth
import Pure.Auth.Data.Username
import Pure.Auth.Data.Password
import Pure.Auth.Data.Token

import Pure.Elm.Component hiding (invalid)
import qualified Pure.Data.LocalStorage as LS
import Pure.Data.Txt
import Pure.WebSocket (WebSocket,request)

import Control.Concurrent (newEmptyMVar,putMVar,takeMVar)

data Login = Login 
  { socket    :: WebSocket
  , onSuccess :: Token -> IO ()
  , onSignup  :: IO ()
  } deriving Theme
  
instance Component Login where
  data Model Login = Model
    { invalid  :: Bool
    , username :: Username
    , password :: Password
    }

  model = Model {..}
    where
      invalid  = False
      username = fromTxt ""
      password = fromTxt ""

  data Msg Login
    = SetUsername Username 
    | SetPassword Password 
    | Submit

  upon = \case
    SetUsername un -> setUsername un
    SetPassword pw -> setPassword pw
    Submit         -> submit

  view Login { onSignup } Model {..} = let status | invalid = Themed @Invalid | otherwise = id in
    Div <| Themed @Login . status |>
      [ Input <| OnInput (withInput (command . SetUsername . fromTxt)) . Placeholder "Username" . Type "name"
      , Input <| OnInput (withInput (command . SetPassword . fromTxt)) . Placeholder "Password" . Type "password"
      , Button <| OnClick (const (command Submit)) |> 
        [ "Log In" ]
      , Button <| OnClick (const onSignup) |> 
        [ "Sign Up" ]
      ]

setUsername :: Username -> Update Login
setUsername un _ mdl = pure mdl { username = un }

setPassword :: Password -> Update Login
setPassword pw _ mdl = pure mdl { password = pw }

submit :: Update Login
submit Login { socket, onSuccess } mdl@Model { username, password } = do
  mv <- newEmptyMVar
  request Auth.api socket Auth.login Auth.LoginRequest {..} (putMVar mv)
  mt <- takeMVar mv
  case mt of
    Nothing -> pure mdl { invalid = True }
    Just t -> do
      LS.put "pure-auth-session" t
      onSuccess t
      pure mdl { invalid = False }

data Invalid deriving Theme