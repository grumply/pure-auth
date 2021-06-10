{-# language LambdaCase, TypeApplications, RecordWildCards, NamedFieldPuns, RankNTypes, DeriveAnyClass, OverloadedStrings, DuplicateRecordFields  #-}
module Pure.Auth.GHCJS.Access.Signup ( signup, Signup (..) ) where

import qualified Pure.Auth.API as Auth
import Pure.Auth.Data.Email
import Pure.Auth.Data.Password
import Pure.Auth.Data.Token
import Pure.Auth.Data.Username

import Pure.Elm.Application hiding (message,name)
import Pure.Data.Txt
import Pure.WebSocket (WebSocket,message)

data Msg 
  = SetUsername Username 
  | SetPassword Password 
  | SetEmail    Email
  | Submit

data Signup = Signup 
  { socket     :: WebSocket
  , onCancel   :: IO () -- call this if user closes the signup modal (not yet implemented)
  , onSuccess  :: IO ()
  , onLogin    :: IO ()
  } deriving Theme

data Model = Model
  { username :: Username
  , password :: Password
  , email    :: Email
  }

signup :: Signup -> View
signup = run (Applet [] [] [] (pure mdl) update view)
  where
    mdl :: Model
    mdl = Model 
      { username = fromTxt ""
      , password = fromTxt ""
      , email    = fromTxt ""
      }

type Update = Elm Msg => Signup -> Model -> IO Model

update :: Msg -> Update
update = \case
  SetUsername un -> setUsername un
  SetPassword pw -> setPassword pw
  SetEmail e     -> setEmail e
  Submit         -> submit

setUsername :: Username -> Update
setUsername un _ mdl = pure mdl { username = un }

setPassword :: Password -> Update
setPassword pw _ mdl = pure mdl { password = pw }

setEmail :: Email -> Update
setEmail em _ mdl = pure mdl { email = em }

submit :: Update
submit Signup { socket, onSuccess } mdl@Model { username, email, password } = do
  message Auth.api socket Auth.register Auth.RegisterMessage {..}
  onSuccess
  pure mdl

type Render = Elm Msg => Model -> View

view :: Signup -> Render
view Signup { onLogin } Model {..} =
  Div <| Themed @Signup |>
    [ Input <| OnInput (withInput (command . SetEmail . fromTxt)) . Placeholder "Email" . Type "email"
    , Input <| OnInput (withInput (command . SetUsername . fromTxt)) . Placeholder "Username" . Type "name"
    , Input <| OnInput (withInput (command . SetPassword . fromTxt)) . Placeholder "Password" . Type "password"
    , Button <| OnClick (const (command Submit)) |> 
      [ "Sign Up" ]
    , Button <| OnClick (const onLogin) |>
      [ "Log In" ]
    ]