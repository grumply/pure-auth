{-# language LambdaCase, NamedFieldPuns, RecordWildCards, BlockArguments, DuplicateRecordFields, PartialTypeSignatures, ScopedTypeVariables, DuplicateRecordFields, TypeApplications #-}
module Pure.Auth.GHC.API (Config(..),auth) where

import Pure.Auth.API as API
import Pure.Auth.GHC.Auth
import Pure.Auth.GHC.Crypto
import Pure.Auth.Data.Email (Email(..))
import Pure.Auth.Data.Hash (Hash(..))
import Pure.Auth.Data.Key (Key(..))
import Pure.Auth.Data.Password (Password(..))
import Pure.Auth.Data.Token (Token(..))
import Pure.Auth.Data.Username (Username,normalize)

import qualified Pure.Data.Txt as Txt
import Pure.WebSocket as WS
import Pure.Sorcerer hiding (event,Deleted)

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (when)
import Data.Typeable
import Prelude hiding (read)

--------------------------------------------------------------------------------
-- Auth API implementation

data Config _role = Config
  { validateUsername :: Username -> Bool
  , onTokenChange    :: Maybe (Token _role) -> IO ()
  , onDeleted        :: Username -> Email -> IO ()
  , onRegister       :: Username -> Email -> Key -> IO () -> IO ()
  , onRecover        :: Username -> Email -> Key -> IO ()
  , onDelete         :: Username -> Email -> Key -> IO ()
  }

auth :: Typeable _role => Config _role -> Endpoints (AuthMessages _role) (AuthRequests _role) (AuthMessages _role) (AuthRequests _role)
auth config = Endpoints API.api msgs reqs
  where
    msgs = handleRegister config
       <:> handleInitiateRecovery config
       <:> handleUpdateEmail config
       <:> handleLogout config
       <:> handleInitiateDelete config
       <:> handleDelete config
       <:> WS.none

    reqs = handleLogin config
       <:> handleActivate config
       <:> handleVerify config
       <:> handleUpdatePassword config
       <:> handleRecover config
       <:> WS.none

handleRegister :: forall _role. Typeable _role => Config _role -> MessageHandler (API.Register _role)
handleRegister Config { onRegister, validateUsername } = awaiting do
  RegisterMessage { email = e, ..} <- acquire

  let un = normalize username

  when (validateUsername un) do
    read (AuthEventStream un :: Stream (AuthEvent _role)) >>= \case

      Just (_ :: Auth _role) -> 
        pure ()

      _ -> do
        k     <- newKey 64
        email <- hashEmail e
        key   <- hashKey k
        pass  <- hashPassword password
        r     <- let username = un in observe (AuthEventStream un :: Stream (AuthEvent _role)) (Registered {..} :: AuthEvent _role)
        let activate = write (AuthEventStream un :: Stream (AuthEvent _role)) (Activated :: AuthEvent _role)
        case r of
          -- Check if `Added` to prevent re-initialization after deletion.
          Added (_ :: Auth _role) -> liftIO (onRegister un e k activate)
          _ -> pure ()

handleInitiateRecovery :: forall _role. Typeable _role => Config _role -> MessageHandler (API.InitiateRecovery _role)
handleInitiateRecovery Config { onRecover } = awaiting do
  InitiateRecoveryMessage { email = e, ..} <- acquire
  
  let un = normalize username

  read (AuthEventStream un :: Stream (AuthEvent _role)) >>= \case

    Just (Auth { email, activation = Nothing } :: Auth _role) | checkHash e email -> do
      k   <- newKey 64
      key <- hashKey k
      write (AuthEventStream un :: Stream (AuthEvent _role)) (StartedRecovery {..} :: AuthEvent _role)
      liftIO (onRecover un e k)
    _ -> 
      pure ()

handleUpdateEmail :: forall _role. Typeable _role => Config _role -> MessageHandler (API.UpdateEmail _role)
handleUpdateEmail Config { onRecover } = awaiting do
  UpdateEmailMessage { email = e, ..} <- acquire

  let un = normalize username

  read (AuthEventStream un :: Stream (AuthEvent _role)) >>= \case

    Just (Auth { activation = Nothing, pass } :: Auth _role) | checkHash password pass -> do
      email <- hashEmail e
      write (AuthEventStream un :: Stream (AuthEvent _role)) (ChangedEmail {..} :: AuthEvent _role)

    _ ->
      pure ()

handleLogout :: forall _role. Typeable _role => Config _role -> MessageHandler (API.Logout _role)
handleLogout Config { onTokenChange } = awaiting do
  LogoutMessage { token = t, ..} <- acquire

  let Token (username,_) = t
      un = normalize username

  read (AuthEventStream un :: Stream (AuthEvent _role)) >>= \case

    Just (Auth { activation = Nothing, tokens } :: Auth _role) | Just token <- checkToken t tokens -> do
      write (AuthEventStream un :: Stream (AuthEvent _role)) (LoggedOut {..} :: AuthEvent _role)
      liftIO (onTokenChange Nothing)

    _ ->
      pure ()

handleLogin :: forall _role. Typeable _role => Config _role -> RequestHandler (API.Login _role)
handleLogin Config { onTokenChange } = responding do
  LoginRequest {..} <- acquire

  let un = normalize username
  
  read (AuthEventStream un :: Stream (AuthEvent _role)) >>= \case

    Just (Auth { activation = Nothing, pass = p } :: Auth _role) | checkHash password p -> do
      t <- newToken un
      reply (Just t)
      token <- hashToken t
      write (AuthEventStream un :: Stream (AuthEvent _role)) (LoggedIn {..} :: AuthEvent _role)
      liftIO (onTokenChange (Just t))

    _ -> 
      reply Nothing

handleActivate :: forall _role. Typeable _role => Config _role -> RequestHandler (API.Activate _role)
handleActivate Config { onTokenChange } = responding do
  ActivateRequest {..} <- acquire

  let un = normalize username

  read (AuthEventStream un :: Stream (AuthEvent _role)) >>= \case

    Just (Auth { activation = Just a } :: Auth _role) | checkHash key a -> do
      t <- newToken un
      reply (Just t)
      token <- hashToken t
      write (AuthEventStream un :: Stream (AuthEvent _role)) (Activated :: AuthEvent _role)
      write (AuthEventStream un :: Stream (AuthEvent _role)) (LoggedIn {..} :: AuthEvent _role)

    _ ->
      reply Nothing

handleVerify :: forall _role. Typeable _role => Config _role -> RequestHandler (API.Verify _role)
handleVerify Config { onTokenChange } = responding do
  VerifyRequest {..} <- acquire

  let Token (username,k) = token
      un = normalize username

  read (AuthEventStream un :: Stream (AuthEvent _role)) >>= \case

    Just (Auth { activation = Nothing, tokens } :: Auth _role) | Just _ <- unsafeCheckHashes k tokens -> do
      reply True
      liftIO (onTokenChange (Just token))

    _ ->
      reply False

handleUpdatePassword :: forall _role. Typeable _role => Config _role -> RequestHandler (API.UpdatePassword _role)
handleUpdatePassword Config { onTokenChange } = responding do
  UpdatePasswordRequest {..} <- acquire

  let un = normalize username

  read (AuthEventStream un :: Stream (AuthEvent _role)) >>= \case

    Just (Auth { activation = Nothing, pass } :: Auth _role) | checkHash oldPassword pass -> do
      t <- newToken un
      reply (Just t)

      pass <- hashPassword newPassword
      write (AuthEventStream un :: Stream (AuthEvent _role)) (ChangedPassword {..} :: AuthEvent _role)

      token <- hashToken t
      write (AuthEventStream un :: Stream (AuthEvent _role)) (LoggedIn {..} :: AuthEvent _role)

      liftIO (onTokenChange (Just t))

    _ ->
      reply Nothing

handleRecover :: forall _role. Typeable _role => Config _role -> RequestHandler (API.Recover _role)
handleRecover Config { onTokenChange } = responding do
  RecoverRequest { key = k, ..} <- acquire

  let un = normalize username

  key <- hashKey k
  read (AuthEventStream un :: Stream (AuthEvent _role)) >>= \case

    Just (Auth { activation = Nothing, recovery = Just r } :: Auth _role) | key == r -> do
      t <- newToken un
      reply (Just t)

      pass <- hashPassword password
      write (AuthEventStream un :: Stream (AuthEvent _role)) (ChangedPassword {..} :: AuthEvent _role)

      token <- hashToken t
      write (AuthEventStream un :: Stream (AuthEvent _role)) (LoggedIn {..} :: AuthEvent _role)
      
      liftIO (onTokenChange (Just t))

    _ ->
      reply Nothing

handleInitiateDelete :: forall _role. Typeable _role => Config _role -> MessageHandler (API.InitiateDeletion _role)
handleInitiateDelete Config { onDelete } = awaiting do
  InitiateDeletionMessage { email = e, ..} <- acquire
  
  let un = normalize username

  read (AuthEventStream un :: Stream (AuthEvent _role)) >>= \case

    Just (Auth { email, deletion = Nothing } :: Auth _role) | checkHash e email -> do
      k   <- newKey 64
      key <- hashKey k
      write (AuthEventStream un :: Stream (AuthEvent _role)) (StartedDeletion {..} :: AuthEvent _role)
      liftIO (onDelete un e k)

    _ -> 
      pure ()

handleDelete :: forall _role. Typeable _role => Config _role -> MessageHandler (API.Delete _role)
handleDelete Config { onDeleted } = awaiting do
  DeleteMessage { email = e, key = k, .. } <- acquire

  let un = normalize username

  read (AuthEventStream un :: Stream (AuthEvent _role)) >>= \case
    Just (Auth { email, deletion = Just k' } :: Auth _role) | checkHash k k' -> do
      write (AuthEventStream un :: Stream (AuthEvent _role)) (Deleted :: AuthEvent _role)
      liftIO (onDeleted un e)
      
    _ ->
      pure ()

