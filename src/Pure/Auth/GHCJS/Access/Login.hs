{-# language LambdaCase, TypeApplications, RecordWildCards, NamedFieldPuns, RankNTypes, DeriveAnyClass, OverloadedStrings, DuplicateRecordFields  #-}
module Pure.Auth.GHCJS.Access.Login ( login, Login(..) ) where

import qualified Pure.Auth.API as Auth
import Pure.Auth.Data.Username
import Pure.Auth.Data.Password
import Pure.Auth.Data.Token

import Pure.Elm.Application hiding (invalid)
import qualified Pure.Data.LocalStorage as LS
import Pure.Data.Txt
import Pure.WebSocket (WebSocket,request)

import Control.Concurrent (newEmptyMVar,putMVar,takeMVar)

data Msg 
  = SetUsername Username 
  | SetPassword Password 
  | Submit

data Login = Login 
  { socket    :: WebSocket
  , onSuccess :: Token -> IO ()
  , onCancel  :: IO () -- call this if user closes the login modal (not yet implemented)
  , onSignup  :: IO ()
  } deriving Theme

data Model = Model
  { invalid  :: Bool
  , username :: Username
  , password :: Password
  }

login :: Login -> View
login = run (Applet [] [] [] (pure mdl) update view)
  where
    mdl :: Model
    mdl = Model 
      { invalid  = False
      , username = fromTxt ""
      , password = fromTxt ""
      }

type Update = Elm Msg => Login -> Model -> IO Model

update :: Msg -> Update
update = \case
  SetUsername un -> setUsername un
  SetPassword pw -> setPassword pw
  Submit         -> submit

setUsername :: Username -> Update
setUsername un _ mdl = pure mdl { username = un }

setPassword :: Password -> Update
setPassword pw _ mdl = pure mdl { password = pw }

submit :: Update
submit Login { socket, onSuccess } mdl@Model { username, password } = do
  mv <- newEmptyMVar
  request Auth.api socket Auth.login Auth.LoginRequest {..} (putMVar mv)
  mt <- takeMVar mv
  case mt of
    Nothing -> pure mdl { invalid = True }
    Just t -> do
      LS.put "session" t
      onSuccess t
      pure mdl { invalid = False }

type Render = Elm Msg => Model -> View

view :: Login -> Render
view Login { onSignup } Model {..} = let status | invalid = Themed @Invalid | otherwise = id in
  Div <| Themed @Login . status |>
    [ Input <| OnInput (withInput (command . SetUsername . fromTxt)) . Placeholder "Username" . Type "name"
    , Input <| OnInput (withInput (command . SetPassword . fromTxt)) . Placeholder "Password" . Type "password"
    , Button <| OnClick (const (command Submit)) |> 
      [ "Log In" ]
    , Button <| OnClick (const onSignup) |> 
      [ "Sign Up" ]
    ]

data Invalid deriving Theme