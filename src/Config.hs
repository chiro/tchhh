{-# LANGUAGE OverloadedStrings #-}

module Config (
  Configuration(..),
  loadConfig,
  saveConfig,
  makeCred
  ) where

import Prelude hiding (takeWhile)

import Control.Applicative ((<$>), (<*>), empty)

import Data.Aeson ((.=), (.:), (.:?))
import qualified Data.Aeson as AE
import Data.Aeson.Types (parseMaybe)
import Data.Default
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Pit (get, setValue)

import Web.Authenticate.OAuth (Credential (..))

pitKey :: T.Text
pitKey = "tchhh"

data Configuration = Configuration {
  isColor          :: Bool,
  oauthToken       :: Maybe T.Text,
  oauthTokenSecret :: Maybe T.Text,
  userId           :: T.Text,
  screenName       :: T.Text,
  consumerToken    :: String,
  consumerSecret   :: String,
  logFile          :: Maybe T.Text
  } deriving (Eq, Show)

instance Default Configuration where
  def = Configuration {
    isColor = False,
    oauthToken = Nothing,
    oauthTokenSecret = Nothing,
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
    v .:? "oauthToken" <*>
    v .:? "oauthTokenSecret" <*>
    v .: "userId" <*>
    v .: "screenName" <*>
    v .: "consumerToken" <*>
    v .: "consumerSecret" <*>
    v .:? "logFile"
  parseJSON _ = Control.Applicative.empty

loadConfig :: IO (Maybe Configuration)
loadConfig = do
  cfg <- get pitKey $ AE.toJSON (def :: Configuration)
  return $ parseMaybe AE.parseJSON cfg

saveConfig :: Configuration -> IO ()
saveConfig = setValue pitKey

makeCred :: Configuration -> Credential
makeCred conf =
  Credential [("oauth_token", TE.encodeUtf8 $ fromJust $ oauthToken conf),
              ("oauth_token_secret", TE.encodeUtf8 $ fromJust $ oauthTokenSecret conf),
              ("user_id", TE.encodeUtf8 $ userId conf),
              ("screen_name", TE.encodeUtf8 $ screenName conf)]
