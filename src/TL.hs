{-# LANGUAGE OverloadedStrings #-}

module TL where

import Web.Twitter.Types

import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as B

import System.IO

import CharacterReference

data Color = Black
           | Red
           | Green
           | Yellow
           | Blue
           | Magenta
           | Cyan
           | White

escape :: Color -> String
escape Black   = "\027[30m"
escape Red     = "\027[31m"
escape Green   = "\027[32m"
escape Yellow  = "\027[33m"
escape Blue    = "\027[34m"
escape Magenta = "\027[35m"
escape Cyan    = "\027[36m"
escape White   = "\027[37m"

mapColor :: Integer -> Color
mapColor 0 = Black
mapColor 1 = Red
mapColor 2 = Green
mapColor 3 = Yellow
mapColor 4 = Blue
mapColor 5 = Magenta
mapColor 6 = Cyan
mapColor 7 = White

showTLwithColor :: StreamingAPI -> IO ()
showTLwithColor (SStatus s) = do
  let user = statusUser s
  putStr . escape . mapColor $ ((userId user `rem` 6) + 1)
  showTL (SStatus s)
  putStr $ escape White
  hFlush stdout
showTLwithColor (SRetweetedStatus rs) = do
  let rtuser = rsUser rs
  putStr . escape . mapColor $ ((userId rtuser `rem` 6) + 1)
  showTL (SRetweetedStatus rs)
  putStr $ escape White
  hFlush stdout
showTLwithColor s = showTL s >> hFlush stdout

showTL :: StreamingAPI -> IO ()
showTL (SStatus s) = do
  let user = statusUser s
      sn = T.encodeUtf8 $ userScreenName user
      cnt = T.encodeUtf8 $ statusText s
  case deref $ B.concat [sn, ": ", cnt] of
    Left _ -> B.putStrLn "some error : deref"
    Right bs -> B.putStrLn $ B.concat bs
showTL (SRetweetedStatus rs) =
  case deref text of
    Left _ -> B.putStrLn "some error : deref"
    RIght bs ->
      B.putStrLn $ B.concat ["Retweeted (by ", T.encodeUtf8 rtuser, "): ",
                             T.encodeUtf8 user, ": ", T.encodeUtf8 bs]
    where rtuser = userScreenName . rsUser $ rs
          status = rsRetweetedStatus rs
          user = userScreenName . statusUser $ status
          text = statusText status
          content = B.concat ["Retweeted (by ", T.encodeUtf8 rtuser, "): ", T.encodeUtf8 user, ": ", T.encodeUtf8 text]
showTL (SEvent ev) = do
  B.putStrLn $ B.concat [B.pack "Event: ", T.encodeUtf8 $ evEvent ev]
  putStr "> "
  showEventTarget $ evTarget ev
  putStr "> "
  showEventTarget $ evSource ev
  maybe (return ()) (\e -> putStr "> " >> showEventTarget e) $ evTargetObject ev
showTL (SDelete d) = do
  putStrLn $ "Delete: " ++ (show $ delUserId d) ++ " " ++ (show $ delId d)
showTL (SFriends f) = do
  putStrLn "sfriends"
showTL (SUnknown v) = return() --do putStrLn $ show v


showEventTarget :: EventTarget -> IO ()
showEventTarget (ETUser u) =
  B.putStrLn $ B.concat ["@", T.encodeUtf8 screenName, " (followers:", fol,
                         ", description:",  description, ")"]
    where screenName = userScreenName u
          description = B.pack (maybe "" show $ userDescription u)
          fol = B.pack $ maybe "" show $ userFollowers u
showEventTarget (ETStatus s) =
  B.putStrLn $ B.concat [T.encodeUtf8 user, ": ", T.encodeUtf8 text]
    where user = userScreenName . statusUser $ s
          text = statusText s
showEventTarget (ETList l) =
  B.putStrLn $ B.concat ["List: ", T.encodeUtf8 fullname, ": ", memberCount]
    where fullname = listFullName $ l
          memberCount = B.pack . show . listMemberCount $ l
showEventTarget o = putStrLn $ "unknown object: " ++ show o

showStatus :: Status -> String
showStatus s = do
  let stxt = B.unpack . T.encodeUtf8 $ statusText s
      sn   = B.unpack . T.encodeUtf8 . userScreenName $ statusUser s
  sn ++ ":" ++ stxt
