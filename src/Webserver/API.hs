{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
-- | hindent doesn't really play well with Servant's type operators, so
-- | just convenience we put the API spec in its own module, where we
-- | can avoid autoformatting.
module Webserver.API
  ( API
  ) where

import Data.Text (Text)
import Servant ((:<|>), (:>), Get, JSON, PlainText)
import Webserver.Types (Status)

type API =
  "healthcheck" :> Get '[ JSON] Status
  :<|>
  "version" :> Get '[ PlainText] Text
