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

import Compilation (CompilationError, compile)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Data.Aeson (encode)
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import Development.GitRev (gitHash)
import Network.Wai (Application)
import Servant
  ( (:<|>)((:<|>))
  , ServantErr
  , err500
  , errBody
  , serve
  , serveDirectoryFileServer
  )
import Servant.Server (Handler, Server)
import Webserver.API (Web)
import Webserver.Types
  ( RPCCall(RPCCallSol2IELEAsm)
  , RPCID
  , RPCResponse(RPCError, RPCSuccess)
  )

api :: Proxy Web
api = Proxy

server :: FilePath -> Server Web
server staticDir =
  version :<|> rpcHandler :<|> serveDirectoryFileServer staticDir

version :: Applicative m => m Text
version = pure $(gitHash)

rpcHandler :: RPCCall -> Handler RPCResponse
rpcHandler (RPCCallSol2IELEAsm rpcID sol2IELEAsm) = do
  result <- liftIO . runStderrLoggingT $ compile sol2IELEAsm
  case result of
    Left errs -> throwError $ toServantError rpcID errs
    Right response -> pure $ RPCSuccess rpcID response

toServantError :: RPCID -> [CompilationError] -> ServantErr
toServantError rpcID errs = err500 {errBody = encode (RPCError rpcID errs)}

app :: FilePath -> Application
app staticDir = serve api (server staticDir)
