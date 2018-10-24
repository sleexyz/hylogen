{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}


import Paths_hylide (getDataFileName)
import Control.Concurrent
import Control.Monad
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as LBS8

import qualified Data.Text as T
import qualified Network.WebSockets as S
import System.Environment (getArgs)
import System.FilePath
import System.FSNotify
import System.Process
import System.Exit (ExitCode(ExitFailure, ExitSuccess))

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200, status404)

import qualified Language.Haskell.Interpreter as I
import qualified Control.Exception as E


data Msg = Err String
         | Code String
         deriving (Show, Generic, ToJSON, FromJSON)

main :: IO ()
main = do
  getArgs >>= \case
    [pathToWatch] -> main' pathToWatch
    _ -> error "Error: Name a file to watch!"

main' :: FilePath ->  IO ()
main' pathToWatch = do
  tid1 <- forkIO serveIndex
  tid2 <- forkIO $ serveGLSL pathToWatch
  putStrLn "Press enter to exit."
  void getLine
  killThread tid1
  killThread tid2



serveGLSL :: FilePath -> IO ()
serveGLSL pathToWatch = do
  withManager
    $ S.runServer "127.0.0.1" 8080
    . handleConnection pathToWatch
  return ()

handleConnection :: FilePath -> WatchManager -> S.PendingConnection -> IO ()
handleConnection pathToWatch mgr pending = do
   let (dirToWatch, _) = splitFileName pathToWatch
   connection <- S.acceptRequest pending

   -- let send = sendTextData connection . T.pack
   let send = S.sendTextData connection
   let update = do
         msg <- getCodeOrError pathToWatch
         send . encode $ msg

   let onChange e = do
         case e of
           Modified _ _ _ -> update
           _ -> return ()
   update
   _ <- watchDir mgr dirToWatch (const True) onChange
   _ <- getLine -- temp hack to keep the socket open
   return ()

interp :: FilePath -> I.InterpreterT IO String
interp fp = do
  I.loadModules [fp]
  I.setImports [takeBaseName fp]
  I.eval "output"

getCodeOrError :: FilePath -> IO Msg
getCodeOrError path = do
  I.runInterpreter (interp path) >>= return . \case
    Left err -> case err of
      I.UnknownError str -> Err str
      I.WontCompile errors -> Err . mconcat $ I.errMsg <$> errors
      I.NotAllowed str -> Err str
      I.GhcException str -> Err str
    Right str -> Code str

-- getCodeOrError' :: FilePath -> IO Msg
-- getCodeOrError' pathToWatch = do
--    -- TODO: more robust paths!:
--    -- c <- readFile pathToWatch
--    let (dirToWatch, _) = splitFileName pathToWatch
--    (ec, stdout, stderr) <- readProcessWithExitCode "runghc" [
--         "-i"++dirToWatch
--       , pathToWatch
--       ] ""
--    case ec of
--      ExitSuccess   -> return (Code stdout)
--      ExitFailure _ -> return (Err stderr)

serveIndex :: IO ()
serveIndex = do
  let port = 5678
  htmlString <- readFile =<< getDataFileName "client/dist-local/index.html"
  jsString <- readFile =<< getDataFileName "client/dist-local/bundle.js"
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
