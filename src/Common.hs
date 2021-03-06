{-# LANGUAGE OverloadedStrings #-}

module Common (
  getProxyEnv,
  authorize,
  getTokens
  ) where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.Resource

import qualified Data.ByteString.Char8 as B
import qualified Data.CaseInsensitive as CI
import qualified Data.Map as M

import Network.HTTP.Conduit
import qualified Network.URI as URI

import System.Environment (getEnvironment)
import System.IO

import Web.Authenticate.OAuth as OA

getTokens :: B.ByteString -- consumer key
             -> B.ByteString -- consumer secret
             -> OA.OAuth
getTokens ct cs =
  def { oauthServerName = "twitter"
      , oauthRequestUri = "https://api.twitter.com/oauth/request_token"
      , oauthAccessTokenUri = "https://api.twitter.com/oauth/access_token"
      , oauthAuthorizeUri = "https://api.twitter.com/oauth/authorize"
      , oauthConsumerKey = ct
      , oauthConsumerSecret = cs
      , oauthSignatureMethod = HMACSHA1
      , oauthCallback = Just "oob"
      }

getPIN :: String -> IO B.ByteString
getPIN url = do
  putStrLn $ "browse URL: " ++ url
  putStr "> what was the PIN twitter provided you with? "
  hFlush stdout
  B.getLine

getProxyEnv :: IO (Maybe Proxy)
getProxyEnv = do
  env <- M.fromList <$> map (\(k,v) -> (CI.mk k, v)) <$> getEnvironment
  let u = M.lookup "https_proxy" env <|>
          M.lookup "http_proxy"  env <|>
          M.lookup "proxy"       env >>= URI.parseURI >>= URI.uriAuthority
  return $ Proxy <$> (B.pack . URI.uriRegName <$> u) <*> (parsePort . URI.uriPort <$> u)
  where
    parsePort :: String -> Int
    parsePort [] = 8080
    parsePort (':':xs) = read xs
    parsePort xs = error $ "port number parse failed " ++ xs

authorize :: Maybe Proxy -> OAuth -> Manager -> ResourceT IO Credential
authorize pr oauth mng = do
  cred <- OA.getTemporaryCredentialProxy pr oauth mng
  let url = OA.authorizeUrl oauth cred
  pin <- liftIO $ getPIN url
  OA.getAccessTokenProxy pr oauth (OA.insert "oauth_verifier" pin cred) mng
