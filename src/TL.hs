{-# LANGUAGE OverloadedStrings #-}

module TL where

import Web.Twitter.Enumerator

import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as B

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
showTLwithColor (SRetweetedStatus rs) = do
  let rtuser = rsUser rs
  putStr . escape . mapColor $ ((userId rtuser `rem` 6) + 1)
  showTL (SRetweetedStatus rs)
  putStr $ escape White
showTLwithColor s = showTL s

showTL :: StreamingAPI -> IO ()
showTL (SStatus s) = do
  let user = statusUser s
      sn = B.pack $ userScreenName user
      cnt = T.encodeUtf8 $ statusText s
  B.putStrLn $ B.concat [sn, ": ", cnt]
showTL (SRetweetedStatus rs) =
  B.putStrLn $ B.concat ["Retweeted (by ", rtuser, "): ", user, ": ", T.encodeUtf8 text]
    where rtuser = B.pack . userScreenName . rsUser $ rs
          status = rsRetweetedStatus rs
          user = B.pack . userScreenName . statusUser $ status
          text = statusText status
showTL (SEvent ev) = do
  putStrLn $ "Event: " ++ evEvent ev
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
  B.putStrLn $ B.concat ["@", screenName, " (followers:", fol, ", description:", T.encodeUtf8 . userDescription $ u, ")"]
    where screenName = B.pack . userScreenName $ u
          fol = B.pack . maybe "" show $ userFollowers u
showEventTarget (ETStatus s) =
  B.putStrLn $ B.concat [user, ": ", T.encodeUtf8 text]
    where user = B.pack . userScreenName . statusUser $ s
          text = statusText s
showEventTarget (ETList l) =
  B.putStrLn $ B.concat ["List: ", fullname, ": ", memberCount]
    where fullname = B.pack . listFullName $ l
          memberCount = B.pack . show . listMemberCount $ l
showEventTarget o = putStrLn $ "unknown object: " ++ show o

showStatus :: Status -> String
showStatus s = do
  let stxt = B.unpack $ T.encodeUtf8 $ statusText s
      sn   = userScreenName $ statusUser s
  sn ++ ":" ++ stxt