{-# LANGUAGE OverloadedStrings #-}

module Common (
  getProxyEnv,
  authorize
  ) where


import Secret (tokens)
import Web.Twitter.Conduit

import Web.Authenticate.OAuth as OA

import qualified Network.URI as URI
import Network.HTTP.Conduit

import qualified Data.Map as M
import qualified Data.CaseInsensitive as CI
import qualified Data.ByteString.Char8 as B

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.Resource

import System.IO
import System.FilePath
import System.Directory
import System.Environment (getEnvironment)

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

authorize :: Maybe Proxy -> OAuth -> Manager -> ResourceT IO Credential
authorize pr oauth mng = do
  cred <- OA.getTemporaryCredentialProxy pr oauth mng
  let url = OA.authorizeUrl oauth cred
  pin <- liftIO $ getPIN url
  OA.getAccessTokenProxy pr oauth (OA.insert "oauth_verifier" (B.pack pin) cred) mng
