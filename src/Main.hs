{-# LANGUAGE OverloadedStrings #-}

module Main where

import Common
import Config
import Secret
import TL

import Control.Monad.Trans

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL

import qualified Data.ByteString.Char8 as B

import Data.Default

import Data.Maybe (fromJust)

import Web.Twitter.Conduit
import Web.Twitter.Types

import Web.Authenticate.OAuth (Credential (..))

isLogging :: Configuration -> Bool
isLogging cfg = case logFile cfg of
  Just _ -> True
  Nothing -> False

logAndShow :: StreamingAPI -> IO ()
logAndShow s = do
  Just cfg <- confFile >>= loadConfig
  if isLogging cfg
   then appendFile ("./" ++ (B.unpack . fromJust $ logFile cfg)) (show s)
    else return ()
  if isColor cfg
   then showTLwithColor s
   else showTL s

loadCfg :: FilePath -> IO Configuration
loadCfg cp = do
  mcfg <- loadConfig cp
  case mcfg of
    Just c -> return c
    Nothing -> createConfig

loadCredential :: Configuration -> Credential
loadCredential cfg =
  Credential [("oauth_token", oauthToken cfg),
              ("oauth_token_secret", oauthTokenSecret cfg),
              ("user_id", Config.userId cfg),
              ("screen_name", screenName cfg)]

withCredential :: Credential -> TW WithToken (C.ResourceT IO) a -> IO a
withCredential cred task = do
  pr <- getProxyEnv
  let env = (setCredential tokens cred def) { twProxy = pr }
  runTW env task

main :: IO ()
main = do
  cf <- confFile
  cfg <- loadCfg cf
  let cred = loadCredential cfg
  pr <- getProxyEnv
  saveConfig cf cfg
  withCredential cred $ do
    src <- userstream
    src C.$$+- CL.mapM_ (lift . lift . logAndShow)
