{-# LANGUAGE OverloadedStrings #-}

module Main where

import Secret
import Config
import Common
import TL

import Control.Monad.Trans

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL

import qualified Data.ByteString.Char8 as B

import Web.Twitter.Conduit
import Web.Twitter.Types

import Web.Authenticate.OAuth (Credential(..))

logAndShow :: StreamingAPI -> IO ()
logAndShow s = do
  Just cfg <- confFile >>= loadConfig
  if isLogging cfg
   then appendFile ("./" ++ (B.unpack $ logFile cfg)) (show s)
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

-- inputLoop :: Configuration -> IO ()
-- inputLoop cfg = do
--   s <- T.getLine
--   case s of
--     "quit" -> return ()
--     ""     -> inputLoop cfg
--     otherwise -> do
--       withConfig cfg . run_ $ statusesUpdate (DTE.encodeUtf8 s) ignore
--       inputLoop cfg

withCredential :: Credential -> TW WithToken (C.ResourceT IO) a -> IO a
withCredential cred task = do
  pr <- getProxyEnv
  let env = (setCredential tokens cred (TWInfo { twToken = NoAuth, twProxy = Nothing})) { twProxy = pr }
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
    src C.$$+- CL.mapM_ (lift . lift . showTL)
