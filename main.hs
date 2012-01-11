{-# LANGUAGE OverloadedStrings #-}

module Main where

import Base
import TL
import Web.Twitter.Enumerator

import Data.Enumerator hiding (map, filter, drop, span, iterate)
import qualified Data.Enumerator.List as EL

import Control.Concurrent

iter :: Iteratee StreamingAPI IO ()
iter = do
  st <- EL.head
  case st of
    Just x -> iter
    Nothing -> return ()

main :: IO ()
main = do
  forkIO $ do withCF $ do run_ $ userstream (EL.mapM (\x -> showTL x >> return x) =$ iter)
  inputLoop
  where inputLoop = do
          s <- getLine
          case s of
            "quit" -> return ()
            _ -> inputLoop
