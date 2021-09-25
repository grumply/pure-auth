{-# language LambdaCase, TypeApplications, RecordWildCards, NamedFieldPuns, RankNTypes, DeriveAnyClass, OverloadedStrings, DuplicateRecordFields, TypeFamilies, FlexibleContexts #-}
module Pure.Auth.GHCJS.Access ( Access(..), authorize, withToken ) where

import Pure.Auth.API as Auth
import Pure.Auth.Data.Token
import qualified Pure.Auth.GHCJS.Access.Login as Login
import qualified Pure.Auth.GHCJS.Access.Signup as Signup

import Pure.Elm.Component hiding (active,not,mode,start)
import qualified Pure.Data.LocalStorage as LS
import Pure.Maybe (producing,consuming)
import Pure.WebSocket

import Control.Concurrent

authorize :: IO (Maybe Token)
authorize = do
  mv <- newEmptyMVar
  publish (Initiate (putMVar mv))
  takeMVar mv

withToken :: (Maybe Token -> View) -> View
withToken = producing authorize . consuming

data Access = Access
  { socket       :: WebSocket
  , extend       :: View -> View
  , onRegistered :: View
  }

data Mode = LoggingIn | SigningUp | SignedUp
instance Component Access where
  data Model Access = Model
    { active :: Maybe (Maybe Token -> IO ())
    , mode   :: Mode
    }
    
  model = Model Nothing LoggingIn
    
  data Msg Access 
    = Startup
    | Initiate (Maybe Token -> IO ())
    | Complete (Maybe Token)
    | SetMode Mode
    | Toggle

  startup = [Startup]

  upon = \case
    Startup    -> start
    Initiate f -> initiate f
    SetMode m  -> setMode m
    Toggle     -> toggle
    Complete t -> complete t

  view Access { socket = s, extend, onRegistered } Model {..}
    | SignedUp <- mode
    = extend $
      Div <| Themed @Access |> 
        [ onRegistered ]
    
    | Just _ <- active
    , SigningUp <- mode 
    = extend $ 
      Div <| Themed @Access |>
        [ run Signup.Signup
            { Signup.socket    = s
            , Signup.onSuccess = command (SetMode SignedUp)
            , Signup.onLogin   = command Toggle
            } 
        ]

    | Just _ <- active
    , LoggingIn <- mode
    = extend $ 
      Div <| Themed @Access |>
        [ run Login.Login
            { Login.socket    = s
            , Login.onSuccess = command . Complete . Just
            , Login.onSignup  = command Toggle
            }
        ]

    | otherwise 
    = SimpleHTML "pure-auth-service" 

start :: Update Access
start _ mdl = do
  subscribe
  pure mdl

initiate :: (Maybe Token -> IO ()) -> Update Access
initiate callback Access { socket } mdl = do
  mt <- LS.get "pure-auth-session"
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
        LS.delete "pure-auth-session"
        pure mdl
          { active = Just callback
          , mode = LoggingIn
          }

setMode :: Mode -> Update Access
setMode m _ mdl =
  pure mdl 
    { mode = m 
    }

toggle :: Update Access
toggle _ mdl = 
  pure mdl 
    { mode = 
      case mode mdl of
        SigningUp -> LoggingIn
        LoggingIn -> SigningUp
        x         -> x
    }

complete :: Maybe Token -> Update Access
complete mt  _ mdl = do
  for (active mdl) ($ mt)
  pure mdl 
    { active = Nothing
    , mode = LoggingIn 
    }

instance Theme Access