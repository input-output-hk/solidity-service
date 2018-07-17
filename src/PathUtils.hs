{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module PathUtils
  ( TaintedPath,mkTaintedPath
  , toSafePath
  ) where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON)
import Data.String (IsString, fromString)
import GHC.Generics (Generic)
import System.FilePath.Posix (isRelative, isValid, splitPath)

{-| Encapsulates the notion that user-supplied paths are an inherent
security risk, and must be sanisted before they can be used safely.

See: https://en.wikipedia.org/wiki/Directory_traversal_attack
-}
newtype TaintedPath =
  TaintedPath FilePath
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON, FromJSONKey)

instance IsString TaintedPath where
  fromString = TaintedPath

mkTaintedPath :: FilePath -> TaintedPath
mkTaintedPath = TaintedPath

toSafePath :: TaintedPath -> Maybe FilePath
toSafePath (TaintedPath userPath) =
  if ".." `notElem` splitPath userPath &&
     "../" `notElem` splitPath userPath &&
     isValid userPath && isRelative userPath
    then Just userPath
    else Nothing
