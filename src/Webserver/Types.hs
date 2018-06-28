{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Webserver.Types where

import Compilation (Sol2IELEAsm)
import Control.Lens (makePrisms)
import Data.Aeson (FromJSON, ToJSON, (.:), parseJSON, withObject)
import Data.Text (Text)
import GHC.Generics (Generic)

newtype RPCCall =
  RPCCallSol2IELEAsm Sol2IELEAsm
  deriving (Show, Eq, Generic)

instance FromJSON RPCCall where
  parseJSON =
    withObject "RPCCall" $ \obj -> do
      method :: Text <- obj .: "method"
      version :: Text <- obj .: "jsonrpc"
      params <- obj .: "params"
      decodeParams method version params
    where
      decodeParams "sol2iele_asm" "2.0" params =
        RPCCallSol2IELEAsm <$> parseJSON params
      decodeParams _ _ _ = fail "method/version not recognised."

makePrisms ''RPCCall

newtype RPCResponse =
  RPCResponse Text
  deriving (Show, Eq, Generic, ToJSON)
