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

import Data.Aeson (object)
import Data.Aeson.Types (ToJSON, toJSON)
import Data.Proxy (Proxy(Proxy))
import Data.Time.Calendar ()
import GHC.Generics (Generic)
import Network.Wai (Application)
import Network.Wai.Handler.Warp ()
import Servant ((:>), Get, JSON, Server, serve)
import System.Directory ()

data Status
  = Good
  | Bad
  deriving (Show, Eq, Generic)

instance ToJSON Status where
  toJSON x = object [("status", encode x)]
    where
      encode Good = "good"
      encode Bad = "bad"

------------------------------------------------------------
type API = "healthcheck" :> Get '[ JSON] Status

api :: Proxy API
api = Proxy

------------------------------------------------------------
server :: Server API
server = pure Good

app :: Application
app = serve api server
