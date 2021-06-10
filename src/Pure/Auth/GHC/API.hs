{-# language LambdaCase, NamedFieldPuns, RecordWildCards, BlockArguments, DuplicateRecordFields, PartialTypeSignatures, ScopedTypeVariables, DuplicateRecordFields #-}
module Pure.Auth.GHC.API (Config(..),auth) where

import Pure.Auth.API as API
import Pure.Auth.GHC.Auth
import Pure.Auth.GHC.Crypto
import Pure.Auth.Data.Email (Email(..))
import Pure.Auth.Data.Hash (Hash(..))
import Pure.Auth.Data.Key (Key(..))
import Pure.Auth.Data.Password (Password(..))
import Pure.Auth.Data.Token (Token(..))
import Pure.Auth.Data.Username (Username)

import Pure.WebSocket as WS
import Sorcerer hiding (event)

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (when)
import Prelude hiding (read)

--------------------------------------------------------------------------------
-- Auth API implementation

data Config = Config
  { validateUsername :: Username -> Bool
  , onTokenChange :: Maybe Token -> IO ()
  , onRegister :: Email -> Key -> IO () -> IO ()
  , onRecover :: Email -> Key -> IO ()
  }

auth :: Config -> _
auth config = Endpoints API.api msgs reqs
  where
    msgs = handleRegister config
       <:> handleInitiateRecovery config
       <:> handleUpdateEmail config
       <:> handleLogout config
       <:> WS.none

    reqs = handleLogin config
       <:> handleActivate config
       <:> handleVerify config
       <:> handleUpdatePassword config
       <:> handleRecover config
       <:> WS.none

handleRegister :: Config -> MessageHandler API.Register
handleRegister Config { onRegister, validateUsername } = awaiting do
  RegisterMessage { email = e, ..} <- acquire

  when (validateUsername username) do
    k     <- newKey 64
    email <- hashEmail e
    key   <- hashKey k
    pass  <- hashPassword password
    r     <- observe (AuthEventStream username) Registered {..}
    let activate = write (AuthEventStream username) Activated
    case r of
      Added (_ :: Auth) -> liftIO (onRegister e k activate)
      _ -> pure ()

handleInitiateRecovery :: Config -> MessageHandler API.InitiateRecovery
handleInitiateRecovery Config { onRecover } = awaiting do
  InitiateRecoveryMessage {..} <- acquire

  read (AuthEventStream username) >>= \case

    Just Auth { activation = Nothing } -> do
      k   <- newKey 64
      key <- hashKey k
      write (AuthEventStream username) StartedRecovery {..}
      liftIO (onRecover email k)

    _ -> 
      pure ()

handleUpdateEmail :: Config -> MessageHandler API.UpdateEmail
handleUpdateEmail Config { onRecover } = awaiting do
  UpdateEmailMessage { email = e, ..} <- acquire

  read (AuthEventStream username) >>= \case

    Just Auth { activation = Nothing, pass } | checkHash password pass -> do
      email <- hashEmail e
      write (AuthEventStream username) ChangedEmail {..}

    _ ->
      pure ()

handleLogout :: Config -> MessageHandler API.Logout
handleLogout Config { onTokenChange } = awaiting do
  LogoutMessage { token = t, ..} <- acquire

  let Token (username,_) = t
  read (AuthEventStream username) >>= \case

    Just Auth { activation = Nothing, tokens } | Just token <- checkToken t tokens -> do
      write (AuthEventStream username) LoggedOut {..}
      liftIO (onTokenChange Nothing)

    _ ->
      pure ()

handleLogin :: Config -> RequestHandler API.Login
handleLogin Config { onTokenChange } = responding do
  LoginRequest {..} <- acquire
  
  read (AuthEventStream username) >>= \case

    Just Auth { activation = Nothing, pass = p } | checkHash password p -> do
      t <- newToken username
      reply (Just t)
      token <- hashToken t
      write (AuthEventStream username) LoggedIn {..}
      liftIO (onTokenChange (Just t))

    _ -> 
      reply Nothing

handleActivate :: Config -> RequestHandler API.Activate
handleActivate Config { onTokenChange } = responding do
  ActivateRequest {..} <- acquire

  read (AuthEventStream username) >>= \case

    Just Auth { activation = Just a } | checkHash key a -> do
      t <- newToken username
      reply (Just t)
      token <- hashToken t
      write (AuthEventStream username) Activated
      write (AuthEventStream username) LoggedIn {..}

    _ ->
      reply Nothing

handleVerify :: Config -> RequestHandler API.Verify
handleVerify Config { onTokenChange } = responding do
  VerifyRequest {..} <- acquire

  let Token (username,k) = token
  read (AuthEventStream username) >>= \case

    Just Auth { activation = Nothing, tokens } | Just _ <- unsafeCheckHashes k tokens -> do
      reply True
      liftIO (onTokenChange (Just token))

    _ ->
      reply False

handleUpdatePassword :: Config -> RequestHandler API.UpdatePassword
handleUpdatePassword Config { onTokenChange } = responding do
  UpdatePasswordRequest {..} <- acquire

  read (AuthEventStream username) >>= \case

    Just Auth { activation = Nothing, pass } | checkHash oldPassword pass -> do
      t <- newToken username
      reply (Just t)

      pass <- hashPassword newPassword
      write (AuthEventStream username) ChangedPassword {..}

      token <- hashToken t
      write (AuthEventStream username) LoggedIn {..}

      liftIO (onTokenChange (Just t))

    _ ->
      reply Nothing

handleRecover :: Config -> RequestHandler API.Recover
handleRecover Config { onTokenChange } = responding do
  RecoverRequest { key = k, ..} <- acquire

  key <- hashKey k
  read (AuthEventStream username) >>= \case

    Just Auth { activation = Nothing, recovery = Just r } | key == r -> do
      t <- newToken username
      reply (Just t)

      pass <- hashPassword password
      write (AuthEventStream username) ChangedPassword {..}

      token <- hashToken t
      write (AuthEventStream username) LoggedIn {..}
      
      liftIO (onTokenChange (Just t))

    _ ->
      reply Nothing

