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
  ( app
  ) where

import Compilation (compile)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Data.Default.Class (def)
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import Development.GitRev (gitHash)
import Network.HTTP.Types (Method)
import Network.Wai (Application)
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
import Webserver.API (Web)
import Webserver.Types
  ( RPCRequest(RPCRequestCompile)
  , RPCResponse(RPCError, RPCSuccess)
  )

instance GenerateList NoContent (Method -> Req NoContent) where
  generateList _ = []

server :: FilePath -> Server Web
server staticDir =
  version :<|> rpcHandler :<|> serveDirectoryFileServer staticDir

version :: Applicative m => m Text
version = pure $(gitHash)

rpcHandler :: RPCRequest -> Handler RPCResponse
rpcHandler (RPCRequestCompile rpcID compilation) = do
  result <- liftIO . runStderrLoggingT $ compile compilation
  case result of
    Left err -> pure $ RPCError rpcID err
    Right response -> pure $ RPCSuccess rpcID response

app :: FilePath -> Application
app staticDir =
  gzip def .
  logStdout . cors (const $ Just policy) . provideOptions webApi . serve webApi $
  server staticDir
  where
    policy = simpleCorsResourcePolicy {corsRequestHeaders = ["content-type"]}
    webApi :: Proxy Web
    webApi = Proxy
