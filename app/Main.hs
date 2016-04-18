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

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200, status404)

-- import System.Random

main :: IO ()
main = getArgs >>= \case
  [pathToWatch] -> main' pathToWatch
  _ -> error "Name a file to watch!"

main' :: FilePath ->  IO ()
main' pathToWatch = withManager $ \mgr -> do
  _ <- forkIO $ serveIndex
  runServer "127.0.0.1" 8080 $ handleConnection pathToWatch mgr

handleConnection :: FilePath -> WatchManager -> PendingConnection -> IO ()
handleConnection pathToWatch mgr pending = do
   let (dirToWatch, fileToWatch) = splitFileName pathToWatch
   connection <- acceptRequest pending

   (sendTextData connection . T.pack) =<< getNewSource pathToWatch

   let onChange e = case e of
         Modified _ _ -> (sendTextData connection . T.pack) =<< getNewSource pathToWatch
         _ -> return ()
   _ <- watchDir mgr dirToWatch (const True) onChange
   _ <- getLine -- temp hack to keep the socket open
   return ()

getNewSource :: FilePath -> IO String
getNewSource pathToWatch = do
   -- TODO: more robust paths!:
   -- c <- readFile pathToWatch
   let (dirToWatch, fileToWatch) = splitFileName pathToWatch
   c <- readProcess "runghc" [
        "-i"++dirToWatch
      , pathToWatch
      ] ""
   putStrLn "updated"
   return c

serveIndex :: IO ()
serveIndex = do
  let port = 5678
  htmlString <- readFile =<< getDataFileName "web/index.html"
  jsString <- readFile =<< getDataFileName "web/entry.js"
  run port $ app htmlString jsString


app :: String -> String -> Application
app htmlString jsString req respond = respond $
  case pathInfo req of
    ["entry.js"] -> serveJS jsString
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
