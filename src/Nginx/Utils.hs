{-# LANGUAGE OverloadedStrings #-}
module Nginx.Utils(intValueDirective, textValueDirective, keyValueDirective,
                   numAndSizeDirective, switchDirective, sizeDirective,
                   size, followedBy, skipComment, directiveSeparator) where

import Control.Applicative
import Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as AP
import Data.Text (Text)
import Data.Char (isSpace)
import Control.Monad (void)

import Nginx.Types (Switch(..),Size(..))

skipComment :: Parser ()
skipComment = char '#' *> (void $ AP.takeWhile (not.isEndOfLine))

directiveSeparator :: Parser ()
directiveSeparator = (some $ skipComment <|> (void $ some space)) *> return ()

textValueDirective :: Text -> Parser Text
textValueDirective name = (string name *> skipSpace *> takeWhile1 (/= ';') <* char ';')

intValueDirective :: Text -> Parser Integer
intValueDirective name = (string name *> skipSpace *> decimal <* skipSpace <* char ';')

keyValueDirective :: Text -> Parser (Text,Text)
keyValueDirective name = do
    key <- (string name *> skipSpace *> takeWhile1 (not.isSpace) <* skipSpace)
    val <- takeWhile1 (/= ';') <* char ';'
    return (key, val)

switchDirective :: Text -> Parser Switch
switchDirective name = do
    string name *> skipSpace
    sw <- (string "on" *> return On) <|> (string "off" *> return Off)
    skipSpace
    char ';'
    return sw

size :: Parser Size
size = Size <$> decimal <*> (char 'k' <|> char 'm')

sizeDirective :: Text -> Parser Size
sizeDirective name = string name *> skipSpace *> size <* skipSpace <* char ';'

numAndSizeDirective :: Text -> Parser (Integer, Size)
numAndSizeDirective name = (,) <$> (string name *> skipSpace *> decimal) <*> (skipSpace *> size <* skipSpace <* char ';')

followedBy :: Alternative f => f a -> f b -> f ([a], b)
followedBy p end = scan
    where scan = ((,) <$> pure [] <*> end) <|> (\x (xs, e) -> (x:xs, e)) <$> p <*> scan
