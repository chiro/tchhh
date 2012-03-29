{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config
import Common
import TL

import Web.Twitter.Enumerator

import System.IO

import Web.Authenticate.OAuth (Credential(..))
import Data.Enumerator hiding (map, filter, drop, span, iterate)
import qualified Data.Enumerator.List as EL

import Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as DTE

import qualified Data.ByteString.Char8 as B

import Control.Concurrent

logIter :: Iteratee String IO ()
logIter = EL.mapM_ (\s -> do Just cfg <- confFile >>= loadConfig
                             if isLogging cfg
                               then appendFile ("./" ++ (B.unpack $ logFile cfg)) s
                               else return ())

showIter :: Enumeratee StreamingAPI String IO ()
showIter = EL.mapM (\x -> do Just cfg <- confFile >>= loadConfig
                             if isColor cfg
                               then showTLwithColor x >> (return $ (show x ++ "\n"))
                               else showTL x >> (return $ (show x ++ "\n")))

ignore :: Iteratee a IO ()
ignore = EL.mapM_ (\s -> return ())

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

withConfig :: Configuration -> TW a -> IO a
withConfig cfg task = do
  pr <- getProxyEnv
  let env = newEnv myOauthToken
  runTW env { twCredential = makeCred cfg, twProxy = pr } $ task


inputLoop :: Configuration -> IO ()
inputLoop cfg = do
  s <- T.getLine
  case s of
    "quit" -> return ()
    ""     -> inputLoop cfg
    otherwise -> do
      withConfig cfg . run_ $ statusesUpdate (DTE.encodeUtf8 s) ignore
      inputLoop cfg

main :: IO ()
main = do
  cf <- confFile
  cfg <- loadCfg cf
  saveConfig cf cfg
  forkIO . withConfig cfg . run_ $ userstream (showIter =$ logIter)
  inputLoop cfg
