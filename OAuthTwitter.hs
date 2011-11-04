module OAuthTwitter where

import OAuth

import Codec.Binary.UTF8.String (encodeString)
import Network.HTTP
import Network.HTTP.Headers
import Network.HTTP.Proxy (parseProxy)
import Network.Browser (browse, request, setProxy)
import Data.Maybe
import Data.List

userTimeline :: OAuth -> [(String,String)] -> IO Request_String
userTimeline oauth param =
  oauthRequest oauth userTimelineURL GET param

friendsTimeline :: OAuth -> IO Request_String
friendsTimeline oauth = do
  oauthRequest oauth friendsTimelineURL GET []

update :: OAuth -> String -> IO Request_String
update oauth status =
  oauthRequest oauth updateURL POST [("status", status)]

listTimeline :: OAuth -> String -> String -> IO Request_String
listTimeline oauth userName listName =
  oauthRequest oauth ("http://api.twitter.com/1/" ++ userName ++ "/lists/" ++ listName ++ "/statuses.json") GET []

requestTokenURL = "https://api.twitter.com/oauth/request_token"
accessTokenURL = "https://api.twitter.com/oauth/access_token"

updateURL = "https://api.twitter.com/1/statuses/update.json"
userTimelineURL = "https://api.twitter.com/1/statuses/user_timeline.json"
friendsTimelineURL = "https://api.twitter.com/1/statuses/friends_timeline.json"

cKey = "JBCyIp0oW2IppbybQ5VIw"
cSecret = "YMfHA02sd4dbFn8jmxvtKNZSl8ZCGzZYS649RuT4YE"
aToken = "131831410-At12TihoHiBSduWqudLF9Gkmx5MYnT2SrauxjoY"
aSecret = "xXajkizooG7rKcTgpFmO16nKKBOUrxbj34wwcwDrnQ"

oauth = OAuth {
  consumerKey = cKey,
  consumerSecret = cSecret,
  accessToken = aToken,
  accessTokenSecret = aSecret
  }

main :: IO ()
main = do
  --rq <- userTimeline oauth [("id", "h_chiro")]
  rq <- update oauth $ encodeString "test"
  rq' <- return $ insertHeaderIfMissing HdrContentLength (show (length (rqBody rq))) rq

  (uri,res) <- browse $ do
    setProxy . fromJust $ parseProxy "10.228.200.14:8080"
    request $ rq'
  putStrLn $ rspBody res