{-# LANGUAGE OverloadedStrings #-}

module CharacterReference (
  deref
  ) where

import Control.Applicative

import qualified Data.Attoparsec.Text as AC
import Data.Attoparsec.Text (parseOnly, string)
import qualified Data.Text as T

reference :: AC.Parser T.Text
reference =
  string "&lt;" >> return "<"
  <|> string "&gt;" >> return ">"
  <|> string "&quot;" >> return "\""

deref :: T.Text -> Either String [T.Text]
deref = parseOnly . AC.many1 $ (reference <|> AC.take 1)
