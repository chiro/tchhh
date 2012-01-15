{-# LANGUAGE OverloadedStrings #-}

module Post where

import Control.Applicative
import Control.Monad.Trans

import Web.Twitter.Enumerator

import qualified Data.ByteString.Char8 as B

import Data.Enumerator hiding (map, filter, drop, span, iterate)

import Network.HTTP.Enumerator
import qualified Network.HTTP.Types as HT

-- almost equal to api in Fetch.hs
apiPost :: String -> HT.Query -> Iteratee B.ByteString IO a -> Iteratee B.ByteString TW a
apiPost url query iter = do
  req <- lift $ apiPostRequest url query
  httpMgr req (handleError iter)
  where
    handleError iter' st@(HT.Status sc _) _ =
      if 200 <= sc && sc < 300
      then iter'
      else throwError $ HTTPStatusCodeException st

-- similar to apiRequest
apiPostRequest :: String -> HT.Query -> TW (Request IO)
apiPostRequest uri query = do
  p <- getProxy
  req <- liftIO $ parseUrl uri >>= \r -> return $ r { method = "POST", -- I changed only here
                                                      queryString = query,
                                                      proxy = p }
  signOAuthTW req
  
-- same function is in Fetch.hs
httpMgr :: Request IO
           -> (HT.Status -> HT.ResponseHeaders -> Iteratee B.ByteString IO a)
           -> Iteratee B.ByteString TW a  
httpMgr req iterf = do
  mgr <- lift getManager
  liftTrans $ http req iterf mgr

update :: B.ByteString -> Iteratee B.ByteString IO a -> Iteratee B.ByteString TW a
update tw iter = apiPost "https://api.twitter.com/1/statuses/update.json" [(B.pack "status",Just tw)] iter
