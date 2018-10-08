{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (void, forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (MonadLogger, logInfoN, runStderrLoggingT)
import Data.Function ((&))
import Data.Monoid ((<>))
import qualified Data.Text as Text
import Development.GitRev (gitHash)
import Network (HostName)
import qualified Network.Monitoring.Riemann.Client as Riemann
import qualified Network.Monitoring.Riemann.Event as Riemann
import qualified Network.Monitoring.Riemann.TCPClient as Riemann
import Network.Wai.Handler.Warp
  ( HostPreference
  , defaultSettings
  , setHost
  , setPort
  )
import Options.Applicative
  ( Parser
  , argument
  , auto
  , customExecParser
  , disambiguate
  , help
  , helper
  , idm
  , info
  , infoOption
  , long
  , metavar
  , option
  , prefs
  , short
  , showDefault
  , str
  , strOption
  , value
  )
import qualified Riemann
import UnliftIO (MonadUnliftIO)
import qualified Webserver

data Command = Run
  { _host :: HostPreference
  , _port :: Int
  , _riemannHost :: HostName
  , _staticDir :: FilePath
  } deriving (Show, Eq)

versionOption :: Parser (a -> a)
versionOption =
  infoOption $(gitHash) (short 'v' <> long "version" <> help "Show the version")

commandParser :: Parser Command
commandParser = do
  _host <-
    strOption
      (short 'b' <> long "bind" <> help "Webserver bind address" <> showDefault <>
       value "127.0.0.1")
  _port <-
    option
      auto
      (short 'p' <> long "port" <> help "Webserver port number" <> showDefault <>
       value 8080)
  _riemannHost <-
    strOption
      (short 'r' <> long "riemann" <> help "Reimann host" <> showDefault <>
       value "localhost")
  _staticDir <-
    argument str (metavar "STATIC_DIR" <> help "Static directory to serve up")
  pure Run {..}

runCommand :: (MonadUnliftIO m, MonadLogger m) => Command -> m ()
runCommand Run {..} = do
  logInfoN . Text.pack $ "Running on " <> show _host <> ":" <> show _port
  riemannClient <- liftIO $ Riemann.tcpClient _riemannHost 5555
  Riemann.sendEvent riemannClient $
    Riemann.ok Riemann.service & Riemann.version & Riemann.description "Startup"
  let healthDelaySeconds = 30
      healthDelayMicros = healthDelaySeconds * 1000 * 1000
  void . liftIO . forkIO . forever $ do
    threadDelay healthDelayMicros
    Riemann.sendEvent riemannClient $
      Riemann.ok (Riemann.service <> " health") & Riemann.version & Riemann.ttl (fromIntegral healthDelaySeconds * 2)
  Webserver.run settings riemannClient _staticDir
  where
    settings = setHost _host . setPort _port $ defaultSettings

main :: IO ()
main = do
  command <-
    customExecParser
      (prefs disambiguate)
      (info (helper <*> versionOption <*> commandParser) idm)
  runStderrLoggingT $ do
    logInfoN $ "Running: " <> Text.pack (show command)
    runCommand command
