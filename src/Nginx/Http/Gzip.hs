{-# LANGUAGE OverloadedStrings, GADTs, StandaloneDeriving, DataKinds, FlexibleInstances, KindSignatures #-}
module Nginx.Http.Gzip (GzipDirective, gzipDirective) where

import Data.Text
import Control.Applicative
import Data.Attoparsec.Text
import Nginx.Types (Context(..), Size, Switch)
import Nginx.Utils

data GzipDirective (a :: Context) where
    GzipDirective :: Text -> GzipDirective a
    GzipCompLevelDirective :: Text -> GzipDirective a
    GzipMinLengthDirective :: Text -> GzipDirective a
    GzipDisableDirective :: Text -> GzipDirective a
    GzipProxiedDirective :: Text -> GzipDirective a
    GzipVaryDirective :: Switch -> GzipDirective a
    GzipTypesDirective :: Text -> GzipDirective a

deriving instance Show (GzipDirective a)
gzipDirective :: Parser (GzipDirective a)
gzipDirective = gzipCompLevel <|> gzipMinLength <|> gzipDisable <|> gzipProxied <|> gzipVary <|> gzipTypes <|> gzip

gzip = GzipDirective <$> textValueDirective "gzip"
gzipCompLevel = GzipCompLevelDirective <$> textValueDirective "gzip_comp_level"
gzipMinLength = GzipMinLengthDirective <$> textValueDirective "gzip_min_length"
gzipDisable = GzipDisableDirective <$> textValueDirective "gzip_disable"
gzipProxied = GzipProxiedDirective <$> textValueDirective "gzip_proxied"
gzipVary = GzipVaryDirective <$> switchDirective "gzip_vary"
gzipTypes = GzipTypesDirective <$> textValueDirective "gzip_types"
