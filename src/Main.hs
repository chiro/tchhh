{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config
import Common
import TL

import Web.Twitter.Enumerator

import System.IO

import Data.Enumerator hiding (map, filter, drop, span, iterate)
import qualified Data.Enumerator.List as EL

import Data.Text as T
import qualified Data.Text.IO as DTI
import qualified Data.Text.Encoding as DTE

import qualified Data.ByteString.Char8 as B

import Control.Concurrent


logIter :: Iteratee String IO ()
logIter = EL.mapM_ (\s -> do Just cfg <- confFile >>= loadConfig
                             if isLogging cfg
                               then appendFile ("./" ++ (B.unpack $ logFile cfg)) s
                               else return ())

showIter :: Enumeratee StreamingAPI String IO ()
showIter = EL.mapM (\x -> showTL x >> (return $ (show x ++ "\n")))

ignore :: Iteratee a IO ()
ignore = EL.mapM_ (\s -> return ())

main :: IO ()
main = do
  forkIO . withConf . run_ . userstream $ (showIter =$ logIter)
  inputLoop
  where
    quit = T.pack "quit"
    empty = T.pack ""
    inputLoop = do
      s <- DTI.getLine
      loop s
    loop s
      | s == quit  = return ()
      | s == empty = inputLoop
      | otherwise  = do withConf . run_ $ statusesUpdate (DTE.encodeUtf8 s) ignore
                        inputLoop
