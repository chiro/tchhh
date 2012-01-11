{-# LANGUAGE OverloadedStrings #-}

module Main where

import Base
import TL
import Web.Twitter.Enumerator

import Data.Aeson hiding (Error)

import Data.Enumerator hiding (map, filter, drop, span, iterate)
import qualified Data.Enumerator.List as EL
import qualified Data.Map as M

import Control.Monad.IO.Class
import Control.Monad
import Control.Monad.Trans
import Control.Applicative

import System.Environment


import Network.HTTP.Enumerator
import qualified Network.HTTP.Types as HT

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

import Control.Concurrent

iter :: Iteratee StreamingAPI IO ()
iter = do
  st <- EL.head
  case st of
    Just _ -> iter
    Nothing -> return ()

main :: IO ()
main = do
  forkIO $ do withCF $ do
                run_ $ userstream (EL.mapM (\x -> showTL x >> return x) =$ iter)
  inputLoop
  where inputLoop = do
          s <- getLine
          case s of
            "quit" -> return ()
            _ -> inputLoop
