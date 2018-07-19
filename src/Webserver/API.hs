{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
-- | hindent doesn't really play well with Servant's type operators, so
-- | just convenience we put the API spec in its own module, where we
-- | can avoid autoformatting.
module Webserver.API
  ( API
  , Web
  ) where

import Data.Text (Text)
import Servant (Raw, Post, ReqBody, (:<|>), (:>), Get, JSON, PlainText)
import Webserver.Types (RPCResponse, RPCRequest)

type Web =
  "version" :> Get '[PlainText, JSON] Text
  :<|>
  "api" :> API
  :<|>
  Raw

type API =
  ReqBody '[JSON] RPCRequest :> Post '[JSON] RPCResponse
