{-# LANGUAGE OverloadedStrings #-}
module Nginx.Utils(intValueDirective, textValueDirective, keyValueDirective, followedBy, skipComment) where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Text (Text)
import Data.Char (isSpace)
import Control.Monad (void)

skipComment :: Parser ()
skipComment = skipSpace *> char '#' *> (void $ takeWhile1 (not.isEndOfLine)) <* skipSpace

textValueDirective :: Text -> Parser Text
textValueDirective name = (string name *> skipSpace *> takeWhile1 (/= ';') <* char ';')

intValueDirective :: Text -> Parser Integer
intValueDirective name = (string name *> skipSpace *> decimal <* skipSpace <* char ';')

keyValueDirective :: Text -> Parser (Text,Text)
keyValueDirective name = do
    key <- (string name *> skipSpace *> takeWhile1 (not.isSpace) <* skipSpace)
    val <- takeWhile1 (/= ';') <* char ';'
    return (key, val)

followedBy :: Alternative f => f a -> f b -> f ([a], b)
followedBy p end = scan
    where scan = ((,) <$> pure [] <*> end) <|> (\x (xs, e) -> (x:xs, e)) <$> p <*> scan
