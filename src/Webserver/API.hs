{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
-- | hindent doesn't really play well with Servant's type operators, so
-- | just convenience we put the API spec in its own module, where we
-- | can avoid autoformatting.
module Webserver.API
  ( API
  ) where

import Data.Text (Text)
import Servant (Post, ReqBody, (:<|>), (:>), Get, JSON, PlainText)
import Webserver.Types (RPCResponse, RPCCall)

type API =
  "version" :> Get '[ PlainText] Text
  :<|>
  ReqBody '[JSON] RPCCall :> Post '[JSON] RPCResponse
