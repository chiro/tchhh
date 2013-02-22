{-# LANGUAGE OverloadedStrings #-}

module CharacterReference (
  deref
  ) where

import Control.Applicative

import qualified Data.Attoparsec.ByteString as AC
import Data.Attoparsec.ByteString.Char8 (anyChar, parseOnly, string)
import Data.ByteString.Char8 (ByteString(..))

reference :: AC.Parser ByteString
reference = (do string "&lt;"
                return "<")
            <|> (do
                    string "&gt;"
                    return ">")
            <|> (do
                    string "&quot;"
                    return "\"")

take1 :: AC.Parser ByteString
take1 = AC.take 1

deref :: ByteString -> Either String [ByteString]
deref = parseOnly . AC.many1 $ (reference <|> take1)
