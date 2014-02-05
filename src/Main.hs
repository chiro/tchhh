{-# LANGUAGE OverloadedStrings #-}

module Main where

import Common
import Config
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
    Nothing -> error "Configuration file is not found!"

withCredential :: Credential -> Configuration -> TW (C.ResourceT (NoLoggingT IO)) a -> NoLoggingT IO a
withCredential cred cfg task = do
  pr <- liftIO getProxyEnv
  let tokens = getTokens (B.pack $ consumerToken cfg) (B.pack $ consumerSecret cfg)
  let env = (setCredential tokens cred def) { twProxy = pr }
  runTW env task

main :: IO ()
main = runNoLoggingT $ do
  cf <- liftIO confFile
  cfg <- liftIO $ loadCfg cf
  let cred = makeCred cfg
  liftIO $ saveConfig cf cfg
  withCredential cred cfg $ do
    src <- userstream
    src C.$$+- CL.mapM_ (liftIO . logAndShow)
