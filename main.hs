{-# LANGUAGE OverloadedStrings #-}

module HTC where
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Codec.Binary.UTF8.String
import Data.Digest.Pure.SHA
import Data.List (sort, intercalate)
import Data.Maybe (fromJust)
import Network.HTTP (RequestMethod, urlEncode, urlEncodeVars)
import Network.HTTP.Base
import Network.HTTP.Headers
import Network.URI (URI, parseURI)
import OAuth

cKey :: String
cKey = "JBCyIp0oW2IppbybQ5VIw"
cSecret :: ByteString
cSecret = "YMfHA02sd4dbFn8jmxvtKNZSl8ZCGzZYS649RuT4YE&"
cSecretStr :: String
cSecretStr = "YMfHA02sd4dbFn8jmxvtKNZSl8ZCGzZYS649RuT4YE&"

test2 :: ByteString
test2 = "POST&https%3A%2F%2Fapi.twitter.com%2Foauth%2Frequest_token&oauth_callback%3Dhttp%253A%252F%252Flocalhost%253A3005%252Fthe_dance%252Fprocess_callback%253Fservice_provider_id%253D11%26oauth_consumer_key%3DGDdmIQH6jhtmLUypg82g%26oauth_nonce%3DQP70eNmVz8jvdPevU3oJD2AfF7R7odC2XJcn4XlZJqk%26oauth_signature_method%3DHMAC-SHA1%26oauth_timestamp%3D1272323042%26oauth_version%3D1.0"


makeSignatureBase :: RequestMethod -> URI  -> [(String,String)] -> String
makeSignatureBase m u sl = show m ++ "&"
                       ++ urlEncode (show u) ++ "&" ++ (urlEncode $ urlEncodeVars (sort sl))

makeSignature m u sl = hmacSha1 cSecret $ BS.pack $ encode $  makeSignatureBase m u sl

reqUri :: URI
reqUri = fromJust $ parseURI "https://api.twitter.com/oauth/request_token"

makeReq :: [(String,String)] -> Request_String
makeReq ps = Request { rqURI = reqUri, rqMethod = POST,
                       rqHeaders = [mkHeader HdrAuthorization q,
                                    mkHeader HdrContentType "application/x-www-form-urlencoded",
                                    mkHeader HdrContentLength (show $ length [])],
                       rqBody = ""
                     }
  where sub (x,y) = BS.concat [paramEncode x,"=\"", paramEncode y,['"']]
        pq = ps ++ [("oauth_consumer_key", cKey), ("oauth_signature_method", "HMAC_SHA1"),
                    ("oauth_version", "1.0"), ("oauth_callback", "oob"), ("oauth_nonce", "ranojw2FjeOdFLioe"),
                    ("oauth_timestamp", "1319124720")]
        q = "OAuth " ++ (intercalate ", " $ map sub (("oauth_signature", showDigest $ makeSignature POST reqUri ps):pq))
        
oauth = OAuth { consumerKey = cKey,
                consumerSecret = cSecretStr,
                accessToken = "",
                accessTokenSecret = "" }

