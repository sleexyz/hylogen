{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

import Data.Monoid
import qualified Data.Text as T
import Network.WebSockets
import System.Environment (getArgs)
import System.FilePath
import System.INotify
import System.Process
import System.Random

main :: IO ()
main = getArgs >>= \case
   [pathToWatch] ->
      runServer "127.0.0.1" 8080 $ handleConnection pathToWatch
   _ -> error "Name a file to watch!"

handleConnection :: FilePath -> PendingConnection -> IO ()
handleConnection pathToWatch pending = do
   let (dirToWatch, fileToWatch) = splitFileName pathToWatch
   inotify <- initINotify
   print inotify
   connection <- acceptRequest pending
   (sendTextData connection . T.pack) =<< getNewSource pathToWatch
   -- withINotify $ \inotify ->
   addWatch inotify [Modify] dirToWatch $ \case
      Modified False (Just f) | f == fileToWatch ->
         (sendTextData connection . T.pack) =<< getNewSource pathToWatch
      _ -> return ()
--  print wd
--  removeWatch wd
--  receiveDataMessage connection
   _ <- getLine -- temp hack to keep the socket open
   return ()

getNewSource :: FilePath -> IO String
getNewSource pathToWatch = do
   -- TODO: more robust paths!:
   let (dirToWatch, fileToWatch) = splitFileName pathToWatch
   c <- readProcess "runghc" [
        "-i"++dirToWatch
      , pathToWatch
      ] ""
   putStrLn c
   return c
{-
   color <- randomRIO (0::Float, 1)
   
   return $ unlines [
        "precision mediump float;"
      , "uniform float time;"
      , "uniform vec3 mouse;"
      , "const float PI = 3.141592653589793238462643383;"
      , "varying vec3 uv;"
      , ""
      , "void main() {"
      , "    gl_FragColor = vec4("++show color++", 0.0, 0.0, 1.0);"
      , "}"
      ]
-}
