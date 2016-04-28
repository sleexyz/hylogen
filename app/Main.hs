{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}


import Paths_hylogen (getDataFileName)
import Control.Concurrent
import qualified Data.ByteString.Lazy.Char8 as LBS8

import qualified Data.Text as T
import Network.WebSockets
import System.Environment (getArgs)
import System.FilePath
import System.FSNotify
import System.Process
import System.Exit (ExitCode(ExitFailure, ExitSuccess))

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200, status404)

-- import System.Random

main :: IO ()
main = getArgs >>= \case
  [pathToWatch] -> main' pathToWatch
  _ -> error "Name a file to watch!"

main' :: FilePath ->  IO ()
main' pathToWatch = do
  _ <- forkIO serveIndex
  withManager
    $ runServer "127.0.0.1" 8080
    . handleConnection pathToWatch

handleConnection :: FilePath -> WatchManager -> PendingConnection -> IO ()
handleConnection pathToWatch mgr pending = do
   let (dirToWatch, _) = splitFileName pathToWatch
   connection <- acceptRequest pending

   let update = do
         maybeNewSource <- getNewSource pathToWatch
         case maybeNewSource of
           Just source -> sendTextData connection . T.pack $ source
           Nothing -> return ()
   update

   let onChange e = case e of
         Modified _ _ -> update
         _ -> return ()
   _ <- watchDir mgr dirToWatch (const True) onChange
   _ <- getLine -- temp hack to keep the socket open
   return ()

getNewSource :: FilePath -> IO (Maybe String)
getNewSource pathToWatch = do
   -- TODO: more robust paths!:
   -- c <- readFile pathToWatch
   let (dirToWatch, fileToWatch) = splitFileName pathToWatch
   (ec, stdout, stderr) <- readProcessWithExitCode "runghc" [
        "-i"++dirToWatch
      , pathToWatch
      ] ""
   case ec of
     ExitSuccess -> do
       putStrLn "updated"
       return (Just stdout)
     ExitFailure i -> do
       putStrLn stderr
       return Nothing

serveIndex :: IO ()
serveIndex = do
  let port = 5678
  htmlString <- readFile =<< getDataFileName "web/index.html"
  jsString <- readFile =<< getDataFileName "web/bundle.js"
  run port $ app htmlString jsString


app :: String -> String -> Application
app htmlString jsString req respond = respond $
  case pathInfo req of
    ["bundle.js"] -> serveJS jsString
    []           -> serveHTML htmlString
    _            -> error404

serveHTML :: String -> Network.Wai.Response
serveHTML htmlString = responseLBS status200 [("Content-Type", "text/html")]
  $ LBS8.pack htmlString

serveJS :: String -> Network.Wai.Response
serveJS jsString = responseLBS status200 [("Content-Type", "application/javascript")] 
  $ LBS8.pack jsString

error404 :: Network.Wai.Response
error404 = responseBuilder status404 [("Content-Type", "text/plain")] "404 - Not Found"
