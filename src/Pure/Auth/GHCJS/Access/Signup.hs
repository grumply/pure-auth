{-# language LambdaCase, TypeApplications, RecordWildCards, NamedFieldPuns, RankNTypes, DeriveAnyClass, OverloadedStrings, DuplicateRecordFields, TypeFamilies, FlexibleContexts, ScopedTypeVariables #-}
module Pure.Auth.GHCJS.Access.Signup ( Signup (..) ) where

import qualified Pure.Auth.API as Auth
import Pure.Auth.Data.Email
import Pure.Auth.Data.Password
import Pure.Auth.Data.Token
import Pure.Auth.Data.Username

import Pure.Elm.Component hiding (message,name)
import Pure.Data.Txt
import Pure.WebSocket (WebSocket,message)

import Data.Typeable

data Signup (_role :: *) = Signup 
  { socket    :: WebSocket
  , onSuccess :: IO ()
  , onLogin   :: IO ()
  } deriving Theme

instance Typeable _role => Component (Signup _role) where
  data Model (Signup _role) = Model
    { username :: Username
    , password :: Password
    , email    :: Email
    }
  
  model = Model {..}
    where
      username = fromTxt ""
      password = fromTxt ""
      email    = fromTxt ""

  data Msg (Signup _role)
    = SetUsername Username 
    | SetPassword Password 
    | SetEmail Email
    | Submit

  upon = \case
    SetUsername un -> setUsername un
    SetPassword pw -> setPassword pw
    SetEmail e     -> setEmail e
    Submit         -> submit

  view Signup { onLogin } Model {..} =
    Form <| Themed @(Signup _role) . OnSubmitWith intercept def |>
      [ Input <| TabIndex 0 . OnInput (withInput (command . SetEmail . fromTxt)) . Placeholder "Email" . Type "email"
      , Input <| TabIndex 0 . OnInput (withInput (command . SetUsername . fromTxt)) . Placeholder "Username" . Type "name"
      , Input <| TabIndex 0 . OnInput (withInput (command . SetPassword . fromTxt)) . Placeholder "Password" . Type "password"
      , Button <| TabIndex 0 . OnClick (const (command Submit)) |> 
        [ "Sign Up" ]
      , Button <| TabIndex 0 . OnClick (const onLogin) |>
        [ "Log In" ]
      ]

setUsername :: Username -> Update (Signup _role)
setUsername un _ mdl = pure mdl { username = un }

setPassword :: Password -> Update (Signup _role)
setPassword pw _ mdl = pure mdl { password = pw }

setEmail :: Email -> Update (Signup _role)
setEmail em _ mdl = pure mdl { email = em }

submit :: forall _role. Typeable _role => Update (Signup _role)
submit Signup { socket, onSuccess } mdl@Model { username, email, password } = do
  message (Auth.api @_role) socket (Auth.register @_role) Auth.RegisterMessage {..}
  onSuccess
  pure mdl