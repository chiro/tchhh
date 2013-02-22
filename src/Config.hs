{-# LANGUAGE OverloadedStrings #-}

module Config (
  Configuration(..),
  loadConfig,
  saveConfig,
  createConfig,
  confFile,
  makeCred
  ) where

import Common
import Secret

import Prelude hiding (takeWhile)

import Control.Applicative ((<$>), (<*>), empty)

import qualified Data.Aeson as AE
import Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Default

import Network.HTTP.Conduit

import System.Directory
import System.FilePath
import System.IO

import Web.Authenticate.OAuth (Credential (..))

data Configuration = Configuration {
  isColor          :: Bool,
  oauthToken       :: ByteString,
  oauthTokenSecret :: ByteString,
  userId           :: ByteString,
  screenName       :: ByteString,
  logFile          :: Maybe ByteString
  } deriving (Eq, Show)

instance Default Configuration where
  def = Configuration {
    isColor = False,
    oauthToken = "no token",
    oauthTokenSecret = "no secret",
    userId = "",
    screenName = "",
    logFile = Nothing
    }

instance AE.ToJSON Configuration where
  toJSON (Configuration isColor oauthToken oauthTokenSecret userId screenName logFile) =
    AE.object [
      "color" AE..= isColor,
      "oauthToken" AE..= oauthToken,
      "oauthTokenSecret" AE..= oauthTokenSecret,
      "userId" AE..= userId,
      "screenName" AE..= screenName,
      "logFile" AE..= logFile
      ]
instance AE.FromJSON Configuration where
  parseJSON (AE.Object v) = Configuration <$>
                         v AE..: "color" <*>
                         v AE..: "oauthToken" <*>
                         v AE..: "oauthTokenSecret" <*>
                         v AE..: "userId" <*>
                         v AE..: "screenName" <*>
                         v AE..:? "logFile"
  parseJSON _ = Control.Applicative.empty

getConfig :: B.ByteString -> Maybe Configuration
getConfig conf = AE.decode (BL.pack (B.unpack conf))

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
saveConfig file cfg = B.writeFile file (B.pack . BL.unpack $ AE.encode cfg)

ensureDirectoryExist :: FilePath -> IO FilePath
ensureDirectoryExist dir = do
  createDirectoryIfMissing True dir
  return dir

confdir :: IO FilePath
confdir = fmap (</> ".tchhh") getHomeDirectory >>= ensureDirectoryExist

confFile :: IO FilePath
confFile = fmap (</> "tchhh.json") confdir

createConfig :: IO Configuration
createConfig = do
  pr <- getProxyEnv
  cred <- withManager $ \mgr -> authorize pr tokens mgr
  return $ setValue def (unCredential cred)
  where
    setValue = Prelude.foldr
               (\(n,v) c ->
                 case n of
                   "oauth_token" -> c { oauthToken = v }
                   "oauth_token_secret" -> c { oauthTokenSecret = v }
                   "screen_name" -> c { screenName = v }
                   "user_id" -> c { userId = v})

makeCred :: Configuration -> Credential
makeCred conf = Credential
                [("oauth_token",oauthToken conf),
                 ("oauth_token_secret",oauthTokenSecret conf),
                 ("user_id",userId conf),
                 ("screen_name",screenName conf)]
