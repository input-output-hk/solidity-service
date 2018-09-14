{-# LANGUAGE TemplateHaskell #-}

module Riemann
  ( service
  , version
  ) where

import Development.GitRev (gitHash)
import Network.Monitoring.Riemann.Event (Event, attribute, attributes)

service :: String
service = "solidity-service"

version :: Event -> Event
version = attributes [attribute "version" (Just $(gitHash))]
