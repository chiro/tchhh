{-# LANGUAGE OverloadedStrings #-}

module Main where

import Common
import Config
import Secret
import TL

import Control.Monad (when)
import Control.Monad.Logger
import Control.Monad.Trans

import qualified Data.ByteString.Char8 as B
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Data.Default
import Data.Maybe (fromJust)
import qualified Data.Text as T

import Web.Authenticate.OAuth (Credential (..))
import Web.Twitter.Conduit
import Web.Twitter.Types

isLogging :: Configuration -> Bool
isLogging cfg = case logFile cfg of
  Just _ -> True
  Nothing -> False

logAndShow :: StreamingAPI -> IO ()
logAndShow s = do
  Just cfg <- confFile >>= loadConfig
  when (isLogging cfg) (appendFile ("./" ++ (T.unpack . fromJust $ logFile cfg)) (show s))
  if isColor cfg
   then showTLwithColor s
   else showTL s

loadCfg :: FilePath -> IO Configuration
loadCfg cp = do
  mcfg <- loadConfig cp
  case mcfg of
    Just c -> return c
    Nothing -> createConfig

withCredential :: Credential -> TW (C.ResourceT (LoggingT IO)) a -> LoggingT IO a
withCredential cred task = do
  pr <- liftIO getProxyEnv
  let env = (setCredential tokens cred def) { twProxy = pr }
  runTW env task

main :: IO ()
main = runStderrLoggingT $ do
  cf <- liftIO confFile
  cfg <- liftIO $ loadCfg cf
  let cred = makeCred cfg
  pr <- liftIO getProxyEnv
  liftIO $ saveConfig cf cfg
  withCredential cred $ do
    src <- userstream
    src C.$$+- CL.mapM_ (\x -> liftIO (logAndShow x))
