{-# LANGUAGE OverloadedStrings #-}

module TL where

import Base
import Web.Twitter.Enumerator

import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as B

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
showTL _ = return ()

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
