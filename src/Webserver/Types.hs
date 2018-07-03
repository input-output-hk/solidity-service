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

import Compilation
  ( Compilation(Compilation, _compiler, _files, _mainFilename)
  , CompilationError
  , Compiler(IELEASM, SolidityIELEABI, SolidityIELEASM)
  )
import Control.Lens (makeLenses, makePrisms)
import Data.Aeson
  ( FromJSON
  , ToJSON
  , Value(String)
  , (.:)
  , (.:?)
  , object
  , parseJSON
  , toJSON
  , withObject
  )
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Vector as Vector
import GHC.Generics (Generic)

newtype RPCID =
  RPCID Integer
  deriving (Show, Eq, ToJSON, FromJSON, Generic)

data RPCCall = RPCCallCompile
  { _rpcCallId :: !(Maybe RPCID)
  , _instructions :: !Compilation
  } deriving (Show, Eq, Generic)

instance FromJSON RPCCall where
  parseJSON =
    withObject "RPCCall" $ \obj -> do
      _rpcCallId <- obj .:? "id"
      method <- obj .: "method"
      _compiler <-
        case method of
          "sol2iele_asm" -> pure SolidityIELEASM
          "sol2iele_abi" -> pure SolidityIELEABI
          "iele_asm" -> pure IELEASM
          _ -> fail $ "method not recognised: " <> method
      params <- traverse parseJSON =<< Vector.toList <$> obj .: "params"
      _instructions <-
        case params of
          [x, y] -> do
            _mainFilename <- parseJSON x
            _files <- parseJSON y
            pure Compilation {..}
          _ -> fail "Invalid payload."
      pure RPCCallCompile {..}

makePrisms ''RPCCall

makeLenses ''RPCCall

data RPCResponse
  = RPCSuccess { _rpcResponseId :: !(Maybe RPCID)
               , _responseBody :: !Text }
  | RPCError { _rpcId :: !(Maybe RPCID)
             , _compilationErrors :: ![CompilationError] }
  deriving (Show, Eq, Generic)

instance ToJSON RPCResponse where
  toJSON (RPCSuccess rpcId text) =
    object
      [("jsonrpc", String "2.0"), ("result", String text), ("id", toJSON rpcId)]
  toJSON (RPCError rpcId errors) =
    object
      [ ("jsonrpc", String "2.0")
      , ("id", toJSON rpcId)
      , ( "error"
        , object
            [ ("code", "-32605")
            , ("message", "Compilation errors")
            , ("data", toJSON errors)
            ])
      ]
