{-# LANGUAGE OverloadedStrings #-}

module Main where

import Base
import TL
import Web.Twitter.Enumerator

import Data.Enumerator hiding (map, filter, drop, span, iterate)
import qualified Data.Enumerator.List as EL

import Control.Concurrent

main :: IO ()
main = do
  forkIO . withCF . run_ . userstream $ EL.mapM_ (\x -> showTL x)
  inputLoop
  where inputLoop = do
          s <- getLine
          case s of
            "quit" -> return ()
            _ -> inputLoop
