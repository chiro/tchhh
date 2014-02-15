{-# LANGUAGE OverloadedStrings #-}

module Config (
  Configuration(..),
  loadConfig,
  saveConfig,
--  createConfig,
  confFile,
  makeCred
  ) where

import Prelude hiding (takeWhile)

import Control.Applicative ((<$>), (<*>), empty)

import Data.Aeson ((.=), (.:), (.:?))
import qualified Data.Aeson as AE
import Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Default
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import System.Directory
import System.FilePath

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

instance AE.ToJSON Configuration where
  toJSON conf =
    AE.object [
      "color" .= isColor conf,
      "oauthToken" .= oauthToken conf,
      "oauthTokenSecret" .= oauthTokenSecret conf,
      "userId" .= userId conf,
      "screenName" .= screenName conf,
      "consumerToken" .= consumerToken conf,
      "consumerSecret" .= consumerSecret conf,
      "logFile" .= logFile conf
      ]

instance AE.FromJSON Configuration where
  parseJSON (AE.Object v) =
    Configuration <$>
    v .: "color" <*>
    v .: "oauthToken" <*>
    v .: "oauthTokenSecret" <*>
    v .: "userId" <*>
    v .: "screenName" <*>
    v .: "consumerToken" <*>
    v .: "consumerSecret" <*>
    v .:? "logFile"
  parseJSON _ = Control.Applicative.empty

loadConfig :: FilePath -> IO (Maybe Configuration)
loadConfig fp = do
  existp <- doesFileExist fp
  if existp
    then do content <- BL.readFile fp
            case AE.decode content of
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
