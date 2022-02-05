{-# language LambdaCase, TypeApplications, RecordWildCards, NamedFieldPuns, RankNTypes, DeriveAnyClass, OverloadedStrings, DuplicateRecordFields, TypeFamilies, FlexibleContexts, ScopedTypeVariables, AllowAmbiguousTypes, UndecidableInstances #-}
module Pure.Auth.GHCJS.Access ( Access(..), authenticate, deauthenticate, authorize, withToken, defaultOnRegistered ) where

import Pure.Auth.API as Auth
import Pure.Auth.Data.Token
import qualified Pure.Auth.GHCJS.Access.Login as Login
import qualified Pure.Auth.GHCJS.Access.Signup as Signup

import Pure.Elm.Component hiding (active,not,mode,start)
import qualified Pure.Data.LocalStorage as LS
import Pure.Hooks
import Pure.Maybe (producing,consuming)
import Pure.WebSocket

import Control.Concurrent
import Control.Monad
import Data.Maybe
import Data.Typeable

authenticate :: forall _role. Typeable _role => WebSocket -> IO (Maybe (Token _role))
authenticate socket = do
  let tc = toTxt (show (typeRepTyCon (typeOf (undefined :: _role))))
  mt <- LS.get ("pure-auth-session-" <> tc)
  case mt of
    Nothing -> pure Nothing
        
    Just t -> do
      mv <- newEmptyMVar
      request (Auth.api @_role) socket (Auth.verify @_role) (Auth.VerifyRequest t) (putMVar mv)
      valid <- takeMVar mv
      if valid then do
        provide (Just t)
        pure (Just t)
      else do
        provide (Nothing :: Maybe (Token _role))
        LS.delete ("pure-auth-session-" <> tc)
        pure Nothing

-- protect a view with a login form, if necessary
authorize :: (Theme (Access _role), Typeable _role) => Access _role -> (Token _role -> View) -> View
authorize access f = useContext' $ \case
  Just (Just t) -> f t
  _             -> run access

deauthenticate :: forall _role. Typeable _role => IO ()
deauthenticate = do
  let tc = toTxt (show (typeRepTyCon (typeOf (undefined :: _role))))
  LS.delete ("pure-auth-session-" <> tc)
  provide (Nothing :: Maybe (Token _role))

-- render a view with role-associated token, updating as the token changes
withToken :: Typeable _role => (Maybe (Token _role) -> View) -> View
withToken f = useContext' (f . join)

data Access (_role :: *) = Access
  { socket       :: WebSocket
  , extend       :: View -> View
  , onRegistered :: IO () -> View
  }

defaultOnRegistered :: IO () -> View
defaultOnRegistered f = producing f (const Null)

data Mode = LoggingIn | SigningUp | SignedUp
instance (Theme (Access _role), Typeable _role) => Component (Access _role) where
  data Model (Access _role) = Model
    { mode :: Mode
    }
    
  model = Model LoggingIn
    
  data Msg (Access _role)
    = Startup
    | SetMode Mode
    | Toggle

  startup = [Startup]

  upon = \case
    Startup    -> start
    SetMode m  -> setMode m
    Toggle     -> toggle

  view Access { socket = s, extend, onRegistered } Model {..} =
    case mode of
      SignedUp -> extend $
        Div <| Themed @Access . Themed @(Access _role) |> 
          [ onRegistered (command Toggle) ]
    
      SigningUp -> extend $ 
        Div <| Themed @Access . Themed @(Access _role) |>
          [ run @(Signup.Signup _role) Signup.Signup
              { Signup.socket    = s
              , Signup.onSuccess = command @(Msg (Access _role)) (SetMode SignedUp)
              , Signup.onLogin   = command @(Msg (Access _role)) Toggle
              } 
          ]

      LoggingIn -> extend $ 
        Div <| Themed @Access . Themed @(Access _role) |>
          [ run @(Login.Login _role) Login.Login
              { Login.socket    = s
              , Login.onSuccess = provide . Just
              , Login.onSignup  = command @(Msg (Access _role)) Toggle
              }
          ]

start :: forall _role. Typeable _role => Update (Access _role)
start Access { socket } mdl = do
  mt <- authenticate @_role socket
  pure mdl

setMode :: Mode -> Update (Access _role)
setMode m _ mdl =
  pure mdl 
    { mode = m 
    }

toggle :: Update (Access _role)
toggle _ mdl = 
  pure mdl 
    { mode = 
      case mode mdl of
        SigningUp -> LoggingIn
        LoggingIn -> SigningUp
        SignedUp  -> LoggingIn
    }

instance Theme Access
instance {-# INCOHERENT #-} Typeable _role => Theme (Access _role)