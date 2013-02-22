{-# LANGUAGE OverloadedStrings #-}

module Base where

import Web.Twitter.Enumerator

import Web.Authenticate.OAuth (OAuth(..), Credential(..))
import qualified Web.Authenticate.OAuth as OA
import qualified Network.URI as URI
import Network.HTTP.Enumerator
import Data.Aeson hiding (Error)
import Data.Aeson.Types (parseMaybe)
import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.ByteString.Char8 as B
import qualified Data.CaseInsensitive as CI
import Data.Attoparsec
import Control.Applicative
import System.IO
import System.FilePath
import System.Directory
import System.Environment (getArgs, getEnvironment)

tokens :: OAuth
tokens = OAuth { oauthServerName = "twitter"
               , oauthRequestUri = "http://twitter.com/oauth/request_token"
               , oauthAccessTokenUri = "http://twitter.com/oauth/access_token"
               , oauthAuthorizeUri = "http://twitter.com/oauth/authorize"
               , oauthConsumerKey = "JBCyIp0oW2IppbybQ5VIw"
               , oauthConsumerSecret = "YMfHA02sd4dbFn8jmxvtKNZSl8ZCGzZYS649RuT4YE"
               , oauthSignatureMethod = OA.HMACSHA1
               , oauthCallback = Nothing
               }
         
ensureDirectoryExist :: FilePath -> IO FilePath
ensureDirectoryExist dir = do
  createDirectoryIfMissing True dir
  return dir

confdir :: IO FilePath
confdir = fmap (</> ".twitter2notify") getHomeDirectory >>= ensureDirectoryExist

credentialFile :: IO FilePath
credentialFile = (</> "credential.json") <$> confdir

withCF :: TW a -> IO a
withCF t = credentialFile >>= \f -> withCredentialFile f t

loadCredential :: FilePath -> IO (Maybe Credential)
loadCredential file = do
  existp <- doesFileExist file
  if existp
    then
    do
      content <- B.readFile file
      return $ (maybeResult . parse json) content >>= parseMaybe parseJSON >>= return . Credential
    else return Nothing

saveCredential :: FilePath -> Credential -> IO ()
saveCredential file cred = LB.writeFile file $ encode . unCredential $ cred

withCredentialFile :: FilePath -> TW a -> IO a
withCredentialFile file task = do
  pr <- getProxyEnv
  cred <- maybe (authorize pr tokens getPIN) return =<< loadCredential file
  saveCredential file cred
  let env = newEnv tokens
  runTW env { twCredential = cred, twProxy = pr } $ task
  where
    getPIN url = do
      putStrLn $ "browse URL: " ++ url
      putStr "> what was the PIN twitter provided you with? "
      hFlush stdout
      getLine

getProxyEnv :: IO (Maybe Proxy)
getProxyEnv = do
  env <- M.fromList <$> map (\(k,v) -> (CI.mk k, v)) <$> getEnvironment
  let u = M.lookup "https_proxy" env <|>
          M.lookup "http_proxy" env <|>
          M.lookup "proxy" env >>= URI.parseURI >>= URI.uriAuthority
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
