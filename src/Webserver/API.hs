{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Webserver.API
  ( API
  , Web
  ) where

import Data.Text (Text)
import Servant ((:<|>), (:>), Get, JSON, PlainText, Post, Raw, ReqBody)
import Webserver.Types (RPCRequest, RPCResponse)

type Web
   = "version" :> Get '[ PlainText, JSON] Text
     :<|> "api" :> API
     :<|> Raw

type API = ReqBody '[ JSON] RPCRequest :> Post '[ JSON] RPCResponse
