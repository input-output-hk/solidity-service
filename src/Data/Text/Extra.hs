module Data.Text.Extra
  ( showt
  ) where

import Data.Text (Text)
import qualified Data.Text as Text

showt :: Show a => a -> Text
showt = Text.pack . show
