-- Signature.hs

module Signature (
  makeSignature,
  ConsumerKey,
  ConsumerSecret,
  AccessToken,
  AccessTokenSecret,
  URL,
  Parameter
  ) where

import Data.Word (Word8(..))
import Data.List
import Network.HTTP (RequestMethod,urlEncode)
import qualified Codec.Binary.Base64 as B64
import Data.Digest.Pure.SHA (hmacSha1, bytestringDigest, showDigest)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

type ConsumerKey = String
type ConsumerSecret = String
type AccessToken = String
type AccessTokenSecret = String

type URL = String
type Parameter = [(String,String)]

-- signatureを生成する関数
makeSignature :: URL -> RequestMethod -> ConsumerSecret -> AccessTokenSecret -> Parameter -> String
makeSignature url method cSecret aSecret param =
    str6
      where
        -- keyの作成
        key = urlEncode cSecret ++ "&" ++ urlEncode aSecret
        -- keyを秘密鍵として、signatureBaseStringで作成したsignature base stringのHMAC-SHA1を取得する
        str4 :: [Word8]
        str4 = L.unpack $ bytestringDigest $ hmacSha1 (L8.pack key) (L8.pack $ makeSignatureBaseString url method param)
        -- str4をBase64エンコードする
        str5 :: String
        str5 = B64.encode str4
        -- str5をURLエンコードする
        str6 = urlEncode str5

-- signature base stringを生成する関数
makeSignatureBaseString :: URL -> RequestMethod -> Parameter -> String
makeSignatureBaseString url method param =
  str3
    where
      -- メソッド(GET, POST)とURLをエンコードした文字列を"&"で連結する
      str1 = show method ++ "&" ++ urlEncode url
      -- paramのキー1=paramの値1&....&paramのキーn=paramの値n
      -- 【重要】パラメータはソートしておかないといけない
      str2 = intercalate "&" $ map concatPair (sort param)
      -- str2をURLエンコードする
      str2' = urlEncode str2
      -- str1とstr2'を"&"で連結する
      str3 = str1 ++ "&" ++ str2'    
      concatPair :: (String, String) -> String
      concatPair (x, y) = x ++ "=" ++ y
