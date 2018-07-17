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
  , CompilationError(CompilationFailed, IOError, InvalidInputPath)
  , Compiler(IELEASM, SolidityIELEABI, SolidityIELEASM)
  )
import Control.Lens (makeLenses, makePrisms)
import Data.Aeson
  ( FromJSON
  , ToJSON
  , (.:)
  , (.:?)
  , object
  , parseJSON
  , toJSON
  , withObject
  )
import Data.Aeson.Types (Parser)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Vector as Vector
import GHC.Generics (Generic)

newtype RPCID =
  RPCID Integer
  deriving (Show, Eq, ToJSON, FromJSON, Generic)

data RPCRequest = RPCRequestCompile
  { _rpcRequestId :: !(Maybe RPCID)
  , _instructions :: !Compilation
  } deriving (Show, Eq, Generic)

instance FromJSON RPCRequest where
  parseJSON =
    withObject "RPCRequest" $ \obj -> do
      _rpcRequestId <- obj .:? "id"
      _compiler <- parseCompiler =<< obj .: "method"
      params <- traverse parseJSON =<< Vector.toList <$> obj .: "params"
      _instructions <-
        case params of
          [x, y] -> do
            _mainFilename <- parseJSON x
            _files <- parseJSON y
            pure Compilation {..}
          _ -> fail "Invalid payload."
      pure RPCRequestCompile {..}
    where
      parseCompiler :: String -> Parser Compiler
      parseCompiler method =
        case method of
          "sol2iele_asm" -> pure SolidityIELEASM
          "sol2iele_abi" -> pure SolidityIELEABI
          "iele_asm" -> pure IELEASM
          _ -> fail $ "method not recognised: " <> method

makePrisms ''RPCRequest

makeLenses ''RPCRequest

data RPCResponse
  = RPCSuccess { _rpcResponseId :: !(Maybe RPCID)
               , _responseBody :: !Text }
  | RPCError { _rpcId :: !(Maybe RPCID)
             , _error :: !CompilationError }
  deriving (Show, Eq, Generic)

instance ToJSON RPCResponse where
  toJSON (RPCSuccess rpcId text) =
    object [("jsonrpc", "2.0"), ("result", toJSON text), ("id", toJSON rpcId)]
  toJSON (RPCError rpcId err) =
    object
      [ ("jsonrpc", "2.0")
      , ("id", toJSON rpcId)
      , ( "error"
        , object
            [ ( "code"
              , case err of
                  InvalidInputPath _ -> "-32602"
                  CompilationFailed _ _ _ -> "-32602"
                  IOError _ -> "-32603")
            , ("message", "Compilation error.")
            , ("data", toJSON err)
            ])
      ]
