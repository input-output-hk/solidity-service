module Main where

import Network.Wai
import Network.Wai.Handler.Warp
import qualified Webserver

main :: IO ()
main = run 8080 Webserver.app
