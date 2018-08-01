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
  , Compiler(IELEASM, SolidityCombinedJSON, SolidityIELEABI,
         SolidityIELEASM, SolidityIELEAST)
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

-- | Note: I don't like this JSON format. Representing the
-- parameters as an unstructured list, instead of an object,
-- really complicates the parsing. :-(
instance FromJSON RPCRequest where
  parseJSON =
    withObject "RPCRequest" $ \obj -> do
      _rpcRequestId <- obj .:? "id"
      method <- obj .: "method"
      params <- traverse parseJSON =<< Vector.toList <$> obj .: "params"
      _instructions <-
        case (method, params) of
          ("isolc_combined_json", [outputTypes, filename, files]) -> do
            _compiler <- SolidityCombinedJSON <$> parseJSON outputTypes
            _mainFilename <- parseJSON filename
            _files <- parseJSON files
            pure Compilation {..}
          (_, [filename, files]) -> do
            _compiler <- parseCompiler method
            _mainFilename <- parseJSON filename
            _files <- parseJSON files
            pure Compilation {..}
          _ -> fail "Invalid payload."
      pure RPCRequestCompile {..}
    where
      parseCompiler :: String -> Parser Compiler
      parseCompiler method =
        case method of
          "sol2iele_asm" -> pure SolidityIELEASM
          "sol2iele_abi" -> pure SolidityIELEABI
          "sol2iele_ast" -> pure SolidityIELEAST
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
  toJSON (RPCError rpcId (CompilationFailed _ _ text)) =
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
                  CompilationFailed {} -> "-32602"
                  IOError _ -> "-32603")
            , ("message", "Compilation error.")
            , ("data", toJSON err)
            ])
      ]
