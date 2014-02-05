{-# LANGUAGE OverloadedStrings #-}

module CharacterReference (
  deref
  ) where

import Control.Applicative

import qualified Data.Attoparsec.ByteString as AC
import Data.Attoparsec.ByteString.Char8 (parseOnly, string)
import Data.ByteString.Char8 (ByteString())

reference :: AC.Parser ByteString
reference =
  string "&lt;" >> return "<"
  <|> string "&gt;" >> return ">"
  <|> string "&quot;" >> return "\""

deref :: ByteString -> Either String [ByteString]
deref = parseOnly . AC.many1 $ (reference <|> AC.take 1)
