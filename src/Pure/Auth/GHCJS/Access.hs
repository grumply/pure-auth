{-# language LambdaCase, TypeApplications, RecordWildCards, NamedFieldPuns, RankNTypes, DeriveAnyClass, OverloadedStrings, DuplicateRecordFields, TypeFamilies, FlexibleContexts, ScopedTypeVariables, AllowAmbiguousTypes #-}
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
import Data.Typeable

authenticate :: Typeable _role => IO (Maybe (Token _role))
authenticate = do
  mv <- newEmptyMVar
  publish (Initiate (putMVar mv))
  takeMVar mv

deauthenticate :: forall _role. Typeable _role => IO ()
deauthenticate = publish (Deauth :: Msg (Access _role))

-- protect a view with a login form, if necessary
authorize :: Typeable _role => (Maybe (Token _role) -> View) -> View
authorize = producing authenticate . consuming

-- render a view with role-associated token, updating as the token changes
withToken :: Typeable _role => (Maybe (Token _role) -> View) -> View
withToken = useContext

data Access (_role :: *) = Access
  { socket       :: WebSocket
  , extend       :: View -> View
  , onRegistered :: View
  }

defaultOnRegistered :: forall _role. Typeable _role => View
defaultOnRegistered = producing (publish (SetMode LoggingIn :: Msg (Access _role))) (const Null)

data Mode = LoggingIn | SigningUp | SignedUp
instance Typeable _role => Component (Access _role) where
  data Model (Access _role) = Model
    { active :: Maybe (Maybe (Token _role) -> IO ())
    , mode   :: Mode
    }
    
  model = Model Nothing LoggingIn
    
  data Msg (Access _role)
    = Startup
    | Initiate (Maybe (Token _role) -> IO ())
    | Complete (Maybe (Token _role))
    | SetMode Mode
    | Toggle
    | Deauth

  startup = [Startup]

  upon = \case
    Startup    -> start
    Initiate f -> initiate f
    SetMode m  -> setMode m
    Toggle     -> toggle
    Complete t -> complete t
    Deauth     -> deauth

  view Access { socket = s, extend, onRegistered } Model {..}
    | SignedUp <- mode
    = extend $
      Div <| Themed @(Access _role) |> 
        [ onRegistered ]
    
    | Just _ <- active
    , SigningUp <- mode 
    = extend $ 
      Div <| Themed @(Access _role) |>
        [ run @(Signup.Signup _role) Signup.Signup
            { Signup.socket    = s
            , Signup.onSuccess = command @(Msg (Access _role)) (SetMode SignedUp)
            , Signup.onLogin   = command @(Msg (Access _role)) Toggle
            } 
        ]

    | Just _ <- active
    , LoggingIn <- mode
    = extend $ 
      Div <| Themed @(Access _role) |>
        [ run @(Login.Login _role) Login.Login
            { Login.socket    = s
            , Login.onSuccess = command @(Msg (Access _role)) . Complete . Just
            , Login.onSignup  = command @(Msg (Access _role)) Toggle
            }
        ]

    | otherwise 
    = SimpleHTML "pure-auth-service" 

start :: Update (Access _role)
start _ mdl = do
  subscribe
  pure mdl

initiate :: forall _role. Typeable _role => (Maybe (Token _role) -> IO ()) -> Update (Access _role)
initiate callback Access { socket } mdl = do
  let tc = toTxt (show (typeRepTyCon (typeOf (undefined :: _role))))
  mt <- LS.get ("pure-auth-session-" <> tc)
  case mt of
    Nothing -> do
      provide mt
      pure mdl 
        { active = Just callback
        , mode = LoggingIn 
        }
        
    Just t -> do
      -- TODO: delegate this to the login form by seeding with this token? 
      --       I'd prefer to keep the API logic in Login/Signup.
      mv <- newEmptyMVar
      request (Auth.api @_role) socket (Auth.verify @_role) (Auth.VerifyRequest t) (putMVar mv)
      valid <- takeMVar mv
      if valid then do
        callback (Just t)
        provide (Just t)
        pure mdl
      else do
        LS.delete ("pure-auth-session-" <> tc)
        provide (Nothing :: Maybe (Token _role))
        pure mdl
          { active = Just callback
          , mode = LoggingIn
          }

deauth :: forall _role. Typeable _role => Update (Access _role)
deauth _ mdl@Model {..} = do
  let tc = toTxt (show (typeRepTyCon (typeOf (undefined :: _role))))
  LS.delete ("pure-auth-session-" <> tc)
  provide (Nothing :: Maybe (Token _role))
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
        x         -> x
    }

complete :: Typeable _role => Maybe (Token _role) -> Update (Access _role)
complete mt  _ mdl = do
  provide mt
  for (active mdl) ($ mt)
  pure mdl 
    { active = Nothing
    , mode = LoggingIn 
    }

instance Typeable _role => Theme (Access _role)