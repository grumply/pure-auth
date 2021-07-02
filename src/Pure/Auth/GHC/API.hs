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

  let un = Txt.toLower username

  when (validateUsername un) do
    k     <- newKey 64
    email <- hashEmail e
    key   <- hashKey k
    pass  <- hashPassword password
    r     <- let username = un in observe (AuthEventStream un) Registered {..}
    let activate = write (AuthEventStream un) Activated
    case r of
      Added (_ :: Auth) -> liftIO (onRegister e k activate)
      _ -> pure ()

handleInitiateRecovery :: Config -> MessageHandler API.InitiateRecovery
handleInitiateRecovery Config { onRecover } = awaiting do
  InitiateRecoveryMessage {..} <- acquire
  
  let un = Txt.toLower username

  read (AuthEventStream un) >>= \case

    Just Auth { activation = Nothing } -> do
      k   <- newKey 64
      key <- hashKey k
      write (AuthEventStream un) StartedRecovery {..}
      liftIO (onRecover email k)

    _ -> 
      pure ()

handleUpdateEmail :: Config -> MessageHandler API.UpdateEmail
handleUpdateEmail Config { onRecover } = awaiting do
  UpdateEmailMessage { email = e, ..} <- acquire

  let un = Txt.toLower username

  read (AuthEventStream un) >>= \case

    Just Auth { activation = Nothing, pass } | checkHash password pass -> do
      email <- hashEmail e
      write (AuthEventStream un) ChangedEmail {..}

    _ ->
      pure ()

handleLogout :: Config -> MessageHandler API.Logout
handleLogout Config { onTokenChange } = awaiting do
  LogoutMessage { token = t, ..} <- acquire

  let un = Txt.toLower username

  let Token (un,_) = t
  read (AuthEventStream un) >>= \case

    Just Auth { activation = Nothing, tokens } | Just token <- checkToken t tokens -> do
      write (AuthEventStream un) LoggedOut {..}
      liftIO (onTokenChange Nothing)

    _ ->
      pure ()

handleLogin :: Config -> RequestHandler API.Login
handleLogin Config { onTokenChange } = responding do
  LoginRequest {..} <- acquire

  let un = Txt.toLower username
  
  read (AuthEventStream un) >>= \case

    Just Auth { activation = Nothing, pass = p } | checkHash password p -> do
      t <- newToken un
      reply (Just t)
      token <- hashToken t
      write (AuthEventStream un) LoggedIn {..}
      liftIO (onTokenChange (Just t))

    _ -> 
      reply Nothing

handleActivate :: Config -> RequestHandler API.Activate
handleActivate Config { onTokenChange } = responding do
  ActivateRequest {..} <- acquire

  let un = Txt.toLower username

  read (AuthEventStream un) >>= \case

    Just Auth { activation = Just a } | checkHash key a -> do
      t <- newToken un
      reply (Just t)
      token <- hashToken t
      write (AuthEventStream un) Activated
      write (AuthEventStream un) LoggedIn {..}

    _ ->
      reply Nothing

handleVerify :: Config -> RequestHandler API.Verify
handleVerify Config { onTokenChange } = responding do
  VerifyRequest {..} <- acquire

  let un = Txt.toLower username

  let Token (un,k) = token
  read (AuthEventStream un) >>= \case

    Just Auth { activation = Nothing, tokens } | Just _ <- unsafeCheckHashes k tokens -> do
      reply True
      liftIO (onTokenChange (Just token))

    _ ->
      reply False

handleUpdatePassword :: Config -> RequestHandler API.UpdatePassword
handleUpdatePassword Config { onTokenChange } = responding do
  UpdatePasswordRequest {..} <- acquire

  let un = Txt.toLower username

  read (AuthEventStream un) >>= \case

    Just Auth { activation = Nothing, pass } | checkHash oldPassword pass -> do
      t <- newToken un
      reply (Just t)

      pass <- hashPassword newPassword
      write (AuthEventStream un) ChangedPassword {..}

      token <- hashToken t
      write (AuthEventStream un) LoggedIn {..}

      liftIO (onTokenChange (Just t))

    _ ->
      reply Nothing

handleRecover :: Config -> RequestHandler API.Recover
handleRecover Config { onTokenChange } = responding do
  RecoverRequest { key = k, ..} <- acquire

  let un = Txt.toLower username

  key <- hashKey k
  read (AuthEventStream un) >>= \case

    Just Auth { activation = Nothing, recovery = Just r } | key == r -> do
      t <- newToken un
      reply (Just t)

      pass <- hashPassword password
      write (AuthEventStream un) ChangedPassword {..}

      token <- hashToken t
      write (AuthEventStream un) LoggedIn {..}
      
      liftIO (onTokenChange (Just t))

    _ ->
      reply Nothing

