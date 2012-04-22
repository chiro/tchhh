{-# LANGUAGE OverloadedStrings #-}

module CharacterReference (
  deref
  ) where

import Data.ByteString.Char8 as B

import Control.Applicative

import Data.Attoparsec.ByteString as AC
import Data.Attoparsec.ByteString.Char8 as AC8 (string, anyChar, parseOnly)

reference :: Parser ByteString
reference = (do AC8.string "&lt;"
                return "<")
            <|> (do
                    AC8.string "&gt;"
                    return ">")
            <|> (do
                    AC8.string "&quot;"
                    return "\"")

take1 :: Parser ByteString
take1 = AC.take 1

deref :: ByteString -> Either String [ByteString]
deref = AC8.parseOnly . AC.many1 $ (reference <|> take1)
