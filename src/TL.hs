{-# LANGUAGE OverloadedStrings #-}

module TL where

import CharacterReference

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Text.IO (putStrLn)

import Prelude hiding (putStrLn)

import System.Console.ANSI
import System.IO (hFlush, stdout)

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

showT :: Show a => a -> T.Text
showT = T.pack . show

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
      sn = userScreenName user
      cnt = statusText s
  case deref $ T.concat [sn, ": ", cnt] of
    Left _ -> putStrLn "some error : deref"
    Right bs -> putStrLn $ T.concat bs
showTL (SRetweetedStatus rs) =
  case deref text of
    Left _ -> putStrLn "some error : deref"
    Right bs ->
      putStrLn $ T.concat ["Retweeted (by ", rtuser, "): ",
                           user, ": ", T.concat bs]
    where rtuser = userScreenName . rsUser $ rs
          status = rsRetweetedStatus rs
          user = userScreenName . statusUser $ status
          text = statusText status

showTL (SEvent ev) = do
  putStrLn $ T.concat ["Event: ", evEvent ev]
  putStr "> "
  showEventTarget $ evTarget ev
  putStr "> "
  showEventTarget $ evSource ev
  maybe (return ()) (\e -> putStr "> " >> showEventTarget e) $ evTargetObject ev
showTL (SDelete d) = putStrLn $ T.concat ["Delete: ", showT (delUserId d), " " ,showT (delId d)]
showTL (SFriends _) = return () -- putStrLn $ show f
showTL (SUnknown _) = return () -- putStrLn $ show v

showEventTarget :: EventTarget -> IO ()
showEventTarget (ETUser u) =
  putStrLn $ T.concat ["@", screenName, " (followers:", showT fol,
                         ", description:", description, ")"]
    where screenName = userScreenName u
          description = fromMaybe "" $ userDescription u
          fol = fromMaybe 0 $ userFollowers u
showEventTarget (ETStatus s) =
  putStrLn $ T.concat [user, ": ", text]
    where user = userScreenName . statusUser $ s
          text = statusText s
showEventTarget (ETList l) =
  putStrLn $ T.concat ["List: ", fullname, ": ", memberCount]
    where fullname = listFullName l
          memberCount = showT $ listMemberCount l
showEventTarget o = putStrLn $ T.concat ["unknown object: ", showT o]

showStatus :: Status -> T.Text
showStatus s = T.concat [userScreenName $ statusUser s, ":", statusText s]
