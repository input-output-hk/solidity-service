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

import Data.Aeson.Types (ToJSON, object, toJSON)
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import Data.Time.Calendar ()
import Development.GitRev (gitHash)
import GHC.Generics (Generic)
import Network.Wai (Application)
import Network.Wai.Handler.Warp ()
import Servant ((:<|>)((:<|>)), (:>), Get, JSON, PlainText, serve)
import Servant.Server
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
type API
   = "healthcheck" :> Get '[ JSON] Status :<|> "version" :> Get '[ PlainText] Text

api :: Proxy API
api = Proxy

------------------------------------------------------------
server :: Server API
server = healthcheck :<|> version

healthcheck :: Applicative m => m Status
healthcheck = pure Good

version :: Applicative m => m Text
version = pure $(gitHash)

app :: Application
app = serve api server
