module OAuth (
  OAuth(..),
  oauthRequest
  ) where

import Signature

import Codec.Binary.UTF8.String (encodeString)
import Network.HTTP
import Network.URI
import Network.HTTP.Proxy (parseProxy)
import Network.Browser (browse,request,setProxy,request)
import Data.Maybe
import Data.List
import System.Time (ClockTime(..), getClockTime)
import Control.Applicative ((<$>))
import System.Random (randomRIO)

data OAuth = OAuth {
  consumerKey :: String,
  consumerSecret :: String,
  accessToken :: String,
  accessTokenSecret :: String
  } deriving (Show, Eq)


randomInt :: IO Int
randomInt = randomRIO (0, maxBound::Int)

getUnixTime :: IO Integer
getUnixTime = getUnixTime' <$> getClockTime
  where
    getUnixTime' (TOD i _) = i

oauthRequest :: OAuth -> URL -> RequestMethod -> [(String,String)] -> IO Request_String
oauthRequest oauth url method param = do
  nonce <- show <$> randomInt
  unixTime <- show <$> getUnixTime
  let
    param' = parameterUrlEncode param
    oauthParam = parameterUrlEncode [("oauth_consumer_key", consumerKey oauth),
                                     ("oauth_nonce", nonce),
                                     ("oauth_signature_method", "HMAC-SHA1"),
                                     ("oauth_timestamp", unixTime),
                                     ("oauth_token", accessToken oauth),
                                     ("oauth_version", "1.0")]
    signature = makeSignature url method (consumerSecret oauth) (accessTokenSecret oauth) (param' ++ oauthParam)
    urlParam = sort (("oauth_signature", signature):(param' ++ oauthParam))
    oauthURL = url ++ "?" ++ (intercalate "&" (map concatParam urlParam))

    concatParam :: (String,String) -> String
    concatParam (x,y) = x ++ "=" ++ y

    parameterUrlEncode :: [(String,String)] -> [(String,String)]
    parameterUrlEncode = map (\(x,y) -> (urlEncode x,urlEncode y))

  return $ Request {
    rqURI = fromJust $ parseURI oauthURL,
    rqMethod = method,
    rqHeaders = [],
    rqBody = ""
             }