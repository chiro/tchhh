{-# LANGUAGE OverloadedStrings #-}

module TL where

import CharacterReference

import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Encoding as T

import System.Console.ANSI
import System.IO

import Web.Twitter.Types

getColor :: Integer -> Color
getColor 0 = Black
getColor 1 = Red
getColor 2 = Green
getColor 3 = Yellow
getColor 4 = Blue
getColor 5 = Magenta
getColor 6 = Cyan
getColor _ = White

getColorIntensity :: Integer -> ColorIntensity
getColorIntensity i = if even i then Dull else Vivid

getSGR :: Integer -> SGR
getSGR user = SetColor Foreground (getColorIntensity user) (getColor (user `rem` 7))

showTLwithColor :: StreamingAPI -> IO ()
showTLwithColor (SStatus s) = do
  let user = statusUser s
  setSGR [getSGR (userId user + 1)]
  showTL (SStatus s)
  setSGR []
  hFlush stdout
showTLwithColor (SRetweetedStatus rs) = do
  let rtuser = rsUser rs
  setSGR [getSGR (userId rtuser + 1)]
  showTL (SRetweetedStatus rs)
  setSGR []
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
  case deref (T.encodeUtf8 text) of
    Left _ -> B.putStrLn "some error : deref"
    Right bs ->
      B.putStrLn $ B.concat ["Retweeted (by ", T.encodeUtf8 rtuser, "): ",
                             T.encodeUtf8 user, ": ", B.concat bs]
    where rtuser = userScreenName . rsUser $ rs
          status = rsRetweetedStatus rs
          user = userScreenName . statusUser $ status
          text = statusText status

showTL (SEvent ev) = do
  B.putStrLn $ B.concat [B.pack "Event: ", T.encodeUtf8 $ evEvent ev]
  putStr "> "
  showEventTarget $ evTarget ev
  putStr "> "
  showEventTarget $ evSource ev
  maybe (return ()) (\e -> putStr "> " >> showEventTarget e) $ evTargetObject ev
showTL (SDelete d) = putStrLn ("Delete: " ++ show (delUserId d) ++ " " ++ show (delId d))
showTL (SFriends _) = return () -- putStrLn $ show f
showTL (SUnknown _) = return () -- putStrLn $ show v


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
    where fullname = listFullName l
          memberCount = B.pack . show $ listMemberCount l
showEventTarget o = putStrLn $ "unknown object: " ++ show o

showStatus :: Status -> String
showStatus s = do
  let stxt = B.unpack . T.encodeUtf8 $ statusText s
      sn   = B.unpack . T.encodeUtf8 . userScreenName $ statusUser s
  sn ++ ":" ++ stxt
