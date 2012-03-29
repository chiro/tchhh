{-# LANGUAGE OverloadedStrings #-}

module Common (
  getProxyEnv,
  myOauthToken,
  authorize
  ) where

import Web.Twitter.Enumerator hiding (userId)

import Web.Authenticate.OAuth (OAuth(..), Credential(..))
import qualified Web.Authenticate.OAuth as OA
import qualified Network.URI as URI
import Network.HTTP.Enumerator

import qualified Data.Map as M
import qualified Data.CaseInsensitive as CI
import qualified Data.ByteString.Char8 as B

import Control.Applicative

import System.IO
import System.FilePath
import System.Directory
import System.Environment (getEnvironment)

myOauthToken :: OAuth
myOauthToken = OAuth { oauthServerName = "twitter"
              , oauthRequestUri = "http://twitter.com/oauth/request_token"
              , oauthAccessTokenUri = "http://twitter.com/oauth/access_token"
              , oauthAuthorizeUri = "http://twitter.com/oauth/authorize"
              , oauthConsumerKey = "JBCyIp0oW2IppbybQ5VIw"
              , oauthConsumerSecret = "YMfHA02sd4dbFn8jmxvtKNZSl8ZCGzZYS649RuT4YE"
              , oauthSignatureMethod = OA.HMACSHA1
              , oauthCallback = Nothing
              }


getPIN :: String -> IO String
getPIN url = do
  putStrLn $ "browse URL: " ++ url
  putStr "> what was the PIN twitter provided you with? "
  hFlush stdout
  getLine

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

authorize :: Maybe Proxy -> OAuth -> IO Credential
authorize pr oauth = do
  cred <- OA.getTemporaryCredentialProxy pr oauth
  let url = OA.authorizeUrl oauth cred
  pin <- getPIN url
  OA.getAccessTokenProxy pr oauth $ OA.insert "oauth_verifier" (B.pack pin) cred
