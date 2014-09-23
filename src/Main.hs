{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Common
import Config
import TL

import Control.Monad (when)
import Control.Monad.Logger
import Control.Monad.Trans
import Control.Monad.Trans.Resource (MonadThrow, monadThrow, ResourceT)

import qualified Data.Aeson as AE
import qualified Data.Aeson.Encode as AEE
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Data.Default
import Data.Maybe (fromJust, isJust)
import qualified Data.Text as T

import Web.Authenticate.OAuth (Credential (..))
import Web.Twitter.Conduit
import Web.Twitter.Types (StreamingAPI(..))

isLogging :: Configuration -> Bool
isLogging = isJust . logFile

logAndShow :: (StreamingAPI, AE.Value) -> IO ()
logAndShow (s, v) = do
  Just cfg <- loadConfig
  when (isLogging cfg) $ do
    let file = "./" ++ (T.unpack . fromJust $ logFile cfg)
    BL.appendFile file $ AEE.encode v
  if isColor cfg
   then showTLwithColor s
   else showTL s

loadCfg :: IO Configuration
loadCfg = do
  mcfg <- loadConfig
  case mcfg of
    Just c -> return c
    Nothing -> error "Configuration file is not found!"

withCredential :: Credential -> Configuration -> TW (ResourceT (NoLoggingT IO)) a -> NoLoggingT IO a
withCredential cred cfg task = do
  pr <- liftIO getProxyEnv
  let tokens = getTokens (B.pack $ consumerToken cfg) (B.pack $ consumerSecret cfg)
  let env = (setCredential tokens cred def) { twProxy = pr }
  runTW env task

($=+) :: MonadIO m
      => C.ResumableSource m a
      -> C.Conduit a m o
      -> m (C.ResumableSource m o)
($=+) = (return .) . (C.$=+)

sinkFromJSONWithRaw :: (AE.FromJSON a, MonadThrow m, MonadLogger m)
                    => C.Consumer B.ByteString m (a, AE.Value)
sinkFromJSONWithRaw = do
  v <- sinkJSON
  case AE.fromJSON v of
    AE.Error err -> monadThrow $ FromJSONError err
    AE.Success r -> return (r, v)

streamWithRaw :: (TwitterBaseM m, AE.FromJSON value)
              => APIRequest apiName responseType
              -> TW m (C.ResumableSource (TW m) (value, AE.Value))
streamWithRaw req = do
    rsrc <- getResponse =<< makeRequest req
    responseBody rsrc $=+ CL.sequence sinkFromJSONWithRaw

main :: IO ()
main = runNoLoggingT $ do
  cfg <- liftIO loadCfg
  let cred = makeCred cfg
  liftIO $ saveConfig cfg
  withCredential cred cfg $ do
    src <- streamWithRaw userstream
    src C.$$+- CL.mapM_ (liftIO . logAndShow)
