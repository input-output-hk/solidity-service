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

import Compilation (CompilationError, compileSol2IELEAsm)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Data.Aeson (encode)
import Data.Bifunctor (bimap)
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import Data.Time.Calendar ()
import Development.GitRev (gitHash)
import Network.Wai (Application)
import Network.Wai.Handler.Warp ()
import Servant ((:<|>)((:<|>)), ServantErr, err500, errBody, serve)
import Servant.Server (Handler, Server)
import System.Directory ()
import Webserver.API (API)
import Webserver.Types (RPCCall(RPCCallSol2IELEAsm), RPCResponse(RPCResponse))

api :: Proxy API
api = Proxy

server :: Server API
server = version :<|> rpcHandler

rpcHandler :: RPCCall -> Handler RPCResponse
rpcHandler (RPCCallSol2IELEAsm sol2IELEAsm) = do
  result <-
    liftIO $
    runStderrLoggingT (bimap id RPCResponse <$> compileSol2IELEAsm sol2IELEAsm)
  case result of
    Left errs -> throwError $ toServantError errs
    Right response -> pure response

toServantError :: [CompilationError] -> ServantErr
toServantError errs = err500 {errBody = encode errs}

version :: Applicative m => m Text
version = pure $(gitHash)

app :: Application
app = serve api server
