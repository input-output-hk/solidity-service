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

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import Data.Time.Calendar ()
import Development.GitRev (gitHash)
import Network.Wai (Application)
import Network.Wai.Handler.Warp ()
import Servant ((:<|>)((:<|>)), serve)
import Servant.Server (Server)
import System.Directory ()
import Webserver.API (API)
import Webserver.Types
  ( RPCCall(RPCCallSol2IELEAsm)
  , RPCResponse(RPCResponse)
  , Status(Good)
  , compileSol2IELEAsm
  )

api :: Proxy API
api = Proxy

server :: Server API
server = healthcheck :<|> version :<|> rpcHandler

rpcHandler :: MonadIO m => RPCCall -> m RPCResponse
rpcHandler (RPCCallSol2IELEAsm sol2IELEAsm) = do
  result <- liftIO $ compileSol2IELEAsm sol2IELEAsm
  case result of
    Left err -> fail err
    Right response -> pure $ RPCResponse response

healthcheck :: Applicative m => m Status
healthcheck = pure Good

version :: Applicative m => m Text
version = pure $(gitHash)

app :: Application
app = serve api server
