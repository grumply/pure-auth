{-# language LambdaCase, TypeApplications, RecordWildCards, NamedFieldPuns, RankNTypes, DeriveAnyClass, OverloadedStrings, DuplicateRecordFields, TypeFamilies, FlexibleContexts, ScopedTypeVariables #-}
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
import Data.Typeable

data Login _role = Login 
  { socket    :: WebSocket
  , onSuccess :: Token _role -> IO ()
  , onSignup  :: IO ()
  } deriving Theme
  
instance Typeable _role => Component (Login _role) where
  data Model (Login _role) = Model
    { invalid  :: Bool
    , username :: Username
    , password :: Password
    }

  model = Model {..}
    where
      invalid  = False
      username = fromTxt ""
      password = fromTxt ""

  data Msg (Login _role)
    = SetUsername Username 
    | SetPassword Password 
    | Submit

  upon = \case
    SetUsername un -> setUsername un
    SetPassword pw -> setPassword pw
    Submit         -> submit

  view Login { onSignup } Model {..} = let status | invalid = Themed @Invalid | otherwise = id in
    Form <| Themed @(Login _role) . status |>
      [ Input <| OnInput (withInput (command . SetUsername . fromTxt)) . Placeholder "Username" . Type "name"
      , Input <| OnInput (withInput (command . SetPassword . fromTxt)) . Placeholder "Password" . Type "password"
      , Button <| OnClick (const (command Submit)) |> 
        [ "Log In" ]
      , Button <| OnClick (const onSignup) |> 
        [ "Sign Up" ]
      ]

setUsername :: Username -> Update (Login _role)
setUsername un _ mdl = pure mdl { username = un }

setPassword :: Password -> Update (Login _role)
setPassword pw _ mdl = pure mdl { password = pw }

submit :: forall _role. Typeable _role => Update (Login _role)
submit Login { socket, onSuccess } mdl@Model { username, password } = do
  let tc = toTxt (show (typeRepTyCon (typeOf (undefined :: _role))))
  mv <- newEmptyMVar
  request (Auth.api @_role) socket (Auth.login @_role) Auth.LoginRequest {..} (putMVar mv)
  mt <- takeMVar mv
  case mt of
    Nothing -> pure mdl { invalid = True }
    Just t -> do
      LS.put ("pure-auth-session-" <> tc) t
      onSuccess t
      pure mdl { invalid = False }

data Invalid deriving Theme