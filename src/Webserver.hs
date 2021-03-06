{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Webserver
  ( run
  ) where

import Compilation (compile)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (LoggingT, MonadLogger, runStderrLoggingT)
import Data.Default.Class (def)
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import Development.GitRev (gitHash)
import Network.HTTP.Types (Method)
import qualified Network.Monitoring.Riemann.Client as Riemann
import Network.Wai (Application)
import Network.Wai.Handler.Warp (Settings, runSettings)
import Network.Wai.Middleware.Cors
  ( cors
  , corsRequestHeaders
  , simpleCorsResourcePolicy
  )
import Network.Wai.Middleware.Gzip (gzip)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai.Middleware.Servant.Options (provideOptions)
import Servant ((:<|>)((:<|>)), serve, serveDirectoryFileServer)
import Servant.Foreign (GenerateList, NoContent, Req, generateList)
import Servant.Server (Handler, Server)
import UnliftIO (MonadUnliftIO)
import Webserver.API (Web)
import Webserver.Types
  ( RPCRequest(RPCRequestCompile)
  , RPCResponse(RPCError, RPCSuccess)
  )

instance GenerateList NoContent (Method -> Req NoContent) where
  generateList _ = []

server ::
     (Riemann.Client (LoggingT IO) client) => client -> FilePath -> Server Web
server riemannClient staticDir =
  version :<|> rpcHandler riemannClient :<|> serveDirectoryFileServer staticDir

version :: Applicative m => m Text
version = pure $(gitHash)

rpcHandler ::
     (Riemann.Client (LoggingT IO) client)
  => client
  -> RPCRequest
  -> Handler RPCResponse
rpcHandler riemannClient (RPCRequestCompile rpcID compilation) = do
  result <- liftIO . runStderrLoggingT $ compile riemannClient compilation
  case result of
    Left err -> pure $ RPCError rpcID err
    Right response -> pure $ RPCSuccess rpcID response

app ::
     (Riemann.Client (LoggingT IO) client) => client -> FilePath -> Application
app riemannClient staticDir =
  gzip def .
  logStdout . cors (const $ Just policy) . provideOptions webApi . serve webApi $
  server riemannClient staticDir
  where
    policy = simpleCorsResourcePolicy {corsRequestHeaders = ["content-type"]}
    webApi :: Proxy Web
    webApi = Proxy

run ::
     ( MonadLogger m
     , MonadUnliftIO m
     , MonadIO m
     , Riemann.Client (LoggingT IO) client
     )
  => Settings
  -> client
  -> FilePath
  -> m ()
run settings riemannClient staticDir =
  liftIO . runSettings settings $ Webserver.app riemannClient staticDir
