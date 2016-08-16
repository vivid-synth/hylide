{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}


import Paths_hylide (getDataFileName)
import Control.Concurrent
import Control.Monad
import Data.Aeson
import Data.String
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as LBS8

import qualified Data.Text as T
import qualified Network.WebSockets as S
import System.Environment (getArgs)
import System.FilePath
import System.FSNotify
import System.Process
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode(ExitFailure, ExitSuccess))

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200, status404)

import Data.Time
import Data.Time.LocalTime

import qualified Language.Haskell.Interpreter as I
import qualified Control.Exception as E




usageStr :: String
usageStr = unlines
           [ "Usage: hylide [--record] pathToWatch"
           , "--record      saves a time-stamped copy of code to .hylide on recompiles"
           ]

main :: IO ()
main =
  let
    folderName = ".hylide"
  in
    getArgs >>= \case
      ["--record", pathToWatch] -> do
        createDirectoryIfMissing True folderName
        main' pathToWatch (saveCodeToFile pathToWatch folderName)

      ["-h"]                    -> putStrLn usageStr

      ["--h"]                   -> putStrLn usageStr

      ["help"]                  -> putStrLn usageStr

      [pathToWatch]             -> main' pathToWatch (pure ())

      _                         -> putStrLn usageStr

main' :: FilePath -> IO () ->  IO ()
main' pathToWatch doIfCompiles = do
  tid1 <- forkIO serveIndex
  tid2 <- forkIO $ serveGLSL pathToWatch doIfCompiles
  putStrLn "Press enter to exit."
  void getLine
  killThread tid1
  killThread tid2



serveGLSL :: FilePath -> IO () -> IO ()
serveGLSL pathToWatch doIfCompiles = do
  withManager
    $ S.runServer "127.0.0.1" 8080
    . handleConnection pathToWatch doIfCompiles
  return ()

data Msg = Err String
         | Code String
         deriving (Show, Generic, ToJSON, FromJSON)

handleConnection :: FilePath -> IO ()
                 -> WatchManager -> S.PendingConnection -> IO ()
handleConnection pathToWatch doIfCompiles mgr pending = do
   let (dirToWatch, _) = splitFileName pathToWatch
   connection <- S.acceptRequest pending

   let send = S.sendTextData connection
   let update = do
         msg <- getCodeOrError pathToWatch
         send . encode $ msg
         case msg of
           Code _ -> doIfCompiles
           Err _ -> pure ()

   let onChange e = do
         case e of
           Modified _ _ -> update
           _ -> return ()
   update
   _ <- watchDir mgr dirToWatch (const True) onChange
   _ <- getLine
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


saveCodeToFile :: FilePath -> FilePath -> IO ()
saveCodeToFile srcPath outPath = do
  src <- readFile srcPath
  time <- formatTime defaultTimeLocale "%Y-%m-%d-%H:%M:%S" <$> (getCurrentTime >>= utcToLocalZonedTime)
  writeFile (outPath ++ "/" ++ time ++ ".hs") src


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
