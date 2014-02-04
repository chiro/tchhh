{-# LANGUAGE OverloadedStrings #-}

module Config (
  Configuration(..),
  loadConfig,
  saveConfig,
--  createConfig,
  confFile,
  makeCred
  ) where

import Common

import Prelude hiding (takeWhile)

import Control.Applicative ((<$>), (<*>), empty)

import qualified Data.Aeson as AE
import Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Default
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Network.HTTP.Conduit

import System.Directory
import System.FilePath
import System.IO

import Web.Authenticate.OAuth (Credential (..))

data Configuration = Configuration {
  isColor          :: Bool,
  oauthToken       :: T.Text,
  oauthTokenSecret :: T.Text,
  userId           :: T.Text,
  screenName       :: T.Text,
  consumerToken    :: String,
  consumerSecret   :: String,
  logFile          :: Maybe T.Text
  } deriving (Eq, Show)

instance Default Configuration where
  def = Configuration {
    isColor = False,
    oauthToken = "no token",
    oauthTokenSecret = "no secret",
    userId = "",
    screenName = "",
    consumerToken = "",
    consumerSecret = "",
    logFile = Nothing
    }

instance AE.ToJSON ByteString where
  toJSON = AE.toJSON . B.unpack

instance AE.ToJSON Configuration where
  toJSON (Configuration isColor oauthToken oauthTokenSecret userId screenName consumerToken consumerSecret logFile) =
    AE.object [
      "color" AE..= isColor,
      "oauthToken" AE..= oauthToken,
      "oauthTokenSecret" AE..= oauthTokenSecret,
      "userId" AE..= userId,
      "screenName" AE..= screenName,
      "consumerToken" AE..= consumerToken,
      "consumerSecret" AE..= consumerSecret,
      "logFile" AE..= logFile
      ]
instance AE.FromJSON Configuration where
  parseJSON (AE.Object v) = Configuration <$>
                         v AE..: "color" <*>
                         v AE..: "oauthToken" <*>
                         v AE..: "oauthTokenSecret" <*>
                         v AE..: "userId" <*>
                         v AE..: "screenName" <*>
                         v AE..: "consumerToken" <*>
                         v AE..: "consumerSecret" <*>
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

-- TODO: Impl.
-- createConfig :: IO Configuration
-- createConfig = do
--   pr <- getProxyEnv
--   cred <- withManager $ \mgr -> authorize pr tokens mgr
--   return $ setValue def (Prelude.map (\(a,b) -> (a, TE.decodeUtf8 b)) (unCredential cred))
--   where
--     setValue = Prelude.foldr
--                (\(n,v) c ->
--                  case n of
--                    "oauth_token" -> c { oauthToken = v }
--                    "oauth_token_secret" -> c { oauthTokenSecret = v }
--                    "screen_name" -> c { screenName = v }
--                    "user_id" -> c { userId = v })

makeCred :: Configuration -> Credential
makeCred conf =
  Credential [("oauth_token", TE.encodeUtf8 $ oauthToken conf),
              ("oauth_token_secret", TE.encodeUtf8 $ oauthTokenSecret conf),
              ("user_id", TE.encodeUtf8 $ userId conf),
              ("screen_name", TE.encodeUtf8 $ screenName conf)]
