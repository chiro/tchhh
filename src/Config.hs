{-# LANGUAGE OverloadedStrings #-}

module Config (
  Configuration(..),
  loadConfig,
  saveConfig,
  createConfig,
  confFile,
  defaultConfig,
  makeCred
  ) where

import Common

import Prelude hiding (takeWhile)

import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString.Char8 as AC8 (takeWhile, skipSpace, isSpace)
import qualified Data.Attoparsec.Combinator as AC

import Data.ByteString.Char8 as B

import Web.Authenticate.OAuth (OAuth(..), Credential(..))

import System.IO
import System.FilePath
import System.Directory

showB :: Bool -> String
showB True = "true"
showB False = "false"

data Configuration = Configuration {
  isColor :: Bool,
  isLogging :: Bool,
  logFile :: ByteString,
  oauthToken :: ByteString,
  oauthTokenSecret :: ByteString,
  userId :: ByteString,
  screenName :: ByteString
  } deriving (Eq)
             
instance Show Configuration where
  show cfg = "color=" ++ showB (isColor cfg) ++ "\n"
             ++ "enableLog=" ++ showB (isLogging cfg) ++ "\n"
             ++ "logFile=" ++ B.unpack (logFile cfg) ++ "\n"
             ++ "oauthToken=" ++ B.unpack (oauthToken cfg) ++ "\n"
             ++ "oauthTokenSecret=" ++ B.unpack (oauthTokenSecret cfg) ++ "\n"
             ++ "userId=" ++ B.unpack (userId cfg) ++ "\n"
             ++ "screenName=" ++ B.unpack (screenName cfg) ++ "\n"

defaultConfig :: Configuration
defaultConfig = Configuration {
  isColor          = False,
  isLogging        = False,
  oauthToken       = "default",
  oauthTokenSecret = "default",
  userId           = "default",
  screenName       = "default", 
  logFile          = "log.txt" }

readBool :: ByteString -> Maybe Bool
readBool "true" = Just True
readBool "false" = Just False
readBool _ = Nothing

constructConfig :: [(ByteString,ByteString)] -> Configuration
constructConfig [] = defaultConfig
constructConfig ((name,val):rest) =
  let conf = constructConfig rest in
  case name of
    "color" -> case readBool val of
      Just b           -> conf { isColor = b }
      Nothing          -> conf
    "enableLog" -> case readBool val of
      Just b           -> conf { isLogging = b }
      Nothing          -> conf
    "oauthToken"       -> conf { oauthToken = val }
    "oauthTokenSecret" -> conf { oauthTokenSecret = val }
    "screenName"       -> conf { screenName = val }
    "userId"           -> conf { userId = val }
    "logFile"          -> conf { logFile = val }

getConfig :: ByteString -> Maybe Configuration
getConfig content =
  case parseOnly configs content of
    Left errmsg -> Nothing
    Right cl    -> Just $ constructConfig cl

configs :: Parser [(ByteString,ByteString)]
configs = AC.many1 keyValue

token :: Parser ByteString
token = AC8.takeWhile (\c -> not (AC8.isSpace c || (c == '=')))

keyValue :: Parser (ByteString,ByteString)
keyValue = do AC8.skipSpace
              name <- token
              AC8.skipSpace
              string "="
              AC8.skipSpace
              val <- token
              return (name,val)

loadConfig :: FilePath -> IO (Maybe Configuration)
loadConfig fp = do
  existp <- doesFileExist fp
  if existp
    then
    do
      content <- B.readFile fp
      case getConfig content of
        Just config -> return $ Just config
        Nothing     -> return Nothing
    else return Nothing

saveConfig :: FilePath -> Configuration -> IO ()
saveConfig file cfg = Prelude.writeFile file $ show cfg

ensureDirectoryExist :: FilePath -> IO FilePath
ensureDirectoryExist dir = do
  createDirectoryIfMissing True dir
  return dir

confdir :: IO FilePath
confdir = fmap (</> ".tchhh") getHomeDirectory >>= ensureDirectoryExist

confFile :: IO FilePath
confFile = fmap (</> "tchhh.conf") confdir

createConfig :: IO Configuration
createConfig = do
  pr <- getProxyEnv
  cred <- authorize pr myOauthToken
  return $ setValue defaultConfig (unCredential cred)
  where
    setValue cfg l =
      Prelude.foldr
      (\(n,v) c ->
        case n of
          "oauth_token" -> c { oauthToken = v }
          "oauth_token_secret" -> c { oauthTokenSecret = v }
          "screen_name" -> c { screenName = v }
          "user_id" -> c { userId = v}) cfg l


makeCred :: Configuration -> Credential
makeCred conf = Credential
                [("oauth_token",oauthToken conf),
                 ("oauth_token_secret",oauthTokenSecret conf),
                 ("user_id",userId conf),
                 ("screen_name",screenName conf)]
