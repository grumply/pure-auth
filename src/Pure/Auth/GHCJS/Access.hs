{-# language LambdaCase, TypeApplications, RecordWildCards, NamedFieldPuns, RankNTypes, DeriveAnyClass, OverloadedStrings, DuplicateRecordFields #-}
module Pure.Auth.GHCJS.Access ( Access(..), access, authorize, withToken ) where

import Pure.Auth.API as Auth
import Pure.Auth.Data.Token
import qualified Pure.Auth.GHCJS.Access.Login as Login
import qualified Pure.Auth.GHCJS.Access.Signup as Signup

import Pure.Elm.Application hiding (active,not,mode)
import qualified Pure.Data.LocalStorage as LS
import Pure.Maybe (producing,consuming)
import Pure.WebSocket

import Control.Concurrent

data Mode = LoggingIn | SigningUp | SignedUp

data Msg 
  = Startup
  | Initiate (Maybe Token -> IO ())
  | Complete (Maybe Token)
  | SetMode Mode
  | Toggle

data Access = Access
  { socket       :: WebSocket
  , extend       :: View -> View
  , onRegistered :: View
  }

data Model = Model
  { active :: Maybe (Maybe Token -> IO ())
  , mode   :: Mode
  }

type Update = Elm Msg => Access -> Model -> IO Model

access :: Access -> View
access = run (Applet [Startup] [] [] (pure init) upon view)
  where init = Model Nothing LoggingIn

upon :: Msg -> Update
upon = \case
  Startup    -> startup
  Initiate f -> initiate f
  SetMode m  -> setMode m
  Toggle     -> toggle
  Complete t -> complete t

startup :: Update
startup _ mdl = do
  subscribe
  pure mdl

initiate :: (Maybe Token -> IO ()) -> Update
initiate callback Access { socket } mdl = do
  mt <- LS.get "session"
  case mt of
    Nothing -> pure mdl 
      { active = Just callback
      , mode = LoggingIn 
      }
      
    Just t -> do
      -- TODO: delegate this to the login form by seeding with this token? 
      --       I'd prefer to keep the API logic in Login/Signup.
      mv <- newEmptyMVar
      request Auth.api socket Auth.verify (Auth.VerifyRequest t) (putMVar mv)
      valid <- takeMVar mv
      if valid then do
        callback (Just t)
        pure mdl
      else do
        LS.delete "session"
        pure mdl
          { active = Just callback
          , mode = LoggingIn
          }

setMode :: Mode -> Update
setMode m _ mdl =
  pure mdl 
    { mode = m 
    }

toggle :: Update
toggle _ mdl = 
  pure mdl 
    { mode = 
      case mode mdl of
        SigningUp -> LoggingIn
        LoggingIn -> SigningUp
        x         -> x
    }

complete :: Maybe Token -> Update
complete mt  _ mdl = do
  for (active mdl) ($ mt)
  pure mdl 
    { active = Nothing
    , mode = LoggingIn 
    }

type Render = Elm Msg => Model -> View

view :: Access -> Render
view Access { socket = s, extend, onRegistered } Model {..}
  | SignedUp <- mode
  = extend $
    Div <| Themed @Access |> 
      [ onRegistered ]
  
  | Just _ <- active
  , SigningUp <- mode 
  = extend $ 
    Div <| Themed @Access |>
      [ Signup.signup Signup.Signup
          { Signup.socket    = s
          , Signup.onCancel  = command (Complete Nothing)
          , Signup.onSuccess = command (SetMode SignedUp)
          , Signup.onLogin   = command Toggle
          } 
      ]

  | Just _ <- active
  , LoggingIn <- mode
  = extend $ 
    Div <| Themed @Access |>
      [ Login.login Login.Login
          { Login.socket    = s
          , Login.onCancel  = command (Complete Nothing)
          , Login.onSuccess = command . Complete . Just
          , Login.onSignup  = command Toggle
          }
      ]

  | otherwise 
  = SimpleHTML "pure-auth-service" 

authorize :: IO (Maybe Token)
authorize = do
  mv <- newEmptyMVar
  publish (Initiate (putMVar mv))
  takeMVar mv

withToken :: (Maybe Token -> View) -> View
withToken = producing authorize . consuming

instance Theme Access