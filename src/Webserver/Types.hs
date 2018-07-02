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

import Compilation (CompilationError, Sol2IELEAsm)
import Control.Lens (makeLenses, makePrisms)
import Data.Aeson
  ( FromJSON
  , ToJSON
  , Value(String)
  , (.:)
  , object
  , parseJSON
  , toJSON
  , withObject
  )
import Data.Text (Text)
import GHC.Generics (Generic)

newtype RPCID =
  RPCID Integer
  deriving (Show, Eq, ToJSON, FromJSON, Generic)

data RPCCall = RPCCallSol2IELEAsm
  { _rpcCallId :: !RPCID
  , _instructions :: !Sol2IELEAsm
  } deriving (Show, Eq, Generic)

instance FromJSON RPCCall where
  parseJSON =
    withObject "RPCCall" $ \obj -> do
      rpcId <- obj .: "id"
      method :: Text <- obj .: "method"
      version :: Text <- obj .: "jsonrpc"
      params <- obj .: "params"
      decodeParams rpcId method version params
    where
      decodeParams rpcId "sol2iele_asm" "2.0" params =
        RPCCallSol2IELEAsm rpcId <$> parseJSON params
            -- elif method == "sol2iele_asm" or method == "sol2iele_abi":
      decodeParams _ _ _ _ = fail "method/version not recognised."

makePrisms ''RPCCall

makeLenses ''RPCCall

data RPCResponse
  = RPCSuccess { _rpcResponseId :: !RPCID
               , _responseBody :: !Text }
  | RPCError { _rpcId :: !RPCID
             , _compilationErrors :: ![CompilationError] }
  deriving (Show, Eq, Generic)

instance ToJSON RPCResponse where
  toJSON (RPCSuccess rpcId text) =
    object
      [("jsonrpc", String "2.0"), ("result", String text), ("id", toJSON rpcId)]
  toJSON (RPCError rpcId errors) =
    object
      [ ("jsonrpc", String "2.0")
      , ("errors", toJSON errors)
      , ("id", toJSON rpcId)
      ]
