{-# LANGUAGE TemplateHaskell #-}

module Main where

import Development.GitRev (gitHash)
import Data.Monoid ((<>))
import Network.Wai.Handler.Warp
import Options.Applicative
  ( Parser
  , auto
  , customExecParser
  , disambiguate
  , help
  , helper
  , idm
  , info
  , infoOption
  , long
  , option
  , prefs
  , short
  , showDefault
  , value
  )
import qualified Webserver

data Command =
  Run Int
  deriving (Show, Eq)

versionOption :: Parser (a -> a)
versionOption =
  (infoOption
     $(gitHash)
     (short 'v' <> long "version" <> help "Show the version"))

commandParser :: Parser Command
commandParser =
  Run <$>
  (option
     auto
     (short 'p' <> long "port" <> help "Webserver port number." <> showDefault <>
      value 8080))

runCommand :: Command -> IO ()
runCommand (Run port) = run port Webserver.app

main :: IO ()
main = do
  command <-
    customExecParser
      (prefs disambiguate)
      (info (helper <*> versionOption <*> commandParser) idm)
  runCommand command
