{-# LANGUAGE OverloadedStrings #-}

module Common (
  withConfiguration,
  withConf,
  initialConfig
  ) where

import Web.Twitter.Enumerator hiding (userId)

import Config

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

token :: OAuth
token = OAuth { oauthServerName = "twitter"
              , oauthRequestUri = "http://twitter.com/oauth/request_token"
              , oauthAccessTokenUri = "http://twitter.com/oauth/access_token"
              , oauthAuthorizeUri = "http://twitter.com/oauth/authorize"
              , oauthConsumerKey = "JBCyIp0oW2IppbybQ5VIw"
              , oauthConsumerSecret = "YMfHA02sd4dbFn8jmxvtKNZSl8ZCGzZYS649RuT4YE"
              , oauthSignatureMethod = OA.HMACSHA1
              , oauthCallback = Nothing
              }

makeCred :: Configuration -> [(B.ByteString,B.ByteString)]
makeCred conf = [("oauth_token",oauthToken conf),
                 ("oauth_token_secret",oauthTokenSecret conf),
                 ("user_id",userId conf),
                 ("screen_name",screenName conf)]

getPIN :: String -> IO String
getPIN url = do
  putStrLn $ "browse URL: " ++ url
  putStr "> what was the PIN twitter provided you with? "
  hFlush stdout
  getLine

initialConfig :: IO Configuration
initialConfig = do
  pr <- getProxyEnv
  cred <- authorize pr token getPIN
  return $ setValue defaultConfig (unCredential cred)
  where
    setValue cfg [] = cfg
    setValue cfg ((name,val):xs) =
      let c = setValue cfg xs in
      case name of
        "oauth_token" -> c { oauthToken = val }
        "oauth_token_secret" -> c { oauthTokenSecret = val }
        "screen_name" -> c { screenName = val }
        "user_id"     -> c { userId     = val }


withConfiguration :: Configuration -> TW a -> IO a
withConfiguration cfg task = do
  pr <- getProxyEnv
  let cred = Credential $ makeCred cfg
  let env = newEnv token
  runTW env { twCredential = cred, twProxy = pr } $ task
  
withConf :: TW a -> IO a  
withConf t = do
  mcfg <- confFile >>= loadConfig
  case mcfg of
    Nothing -> do cfg <- initialConfig
                  confFile >>= \f -> saveConfig f cfg
                  withConfiguration cfg t
    Just cfg -> withConfiguration cfg t

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

authorize :: Maybe Proxy -> OAuth -> (String -> IO String) -> IO Credential
authorize pr oauth getPIN = do
  cred <- OA.getTemporaryCredentialProxy pr oauth
  let url = OA.authorizeUrl oauth cred
  pin <- getPIN url
  OA.getAccessTokenProxy pr oauth $ OA.insert "oauth_verifier" (B.pack pin) cred
