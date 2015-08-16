{-# LANGUAGE OverloadedStrings, GADTs, StandaloneDeriving, DataKinds, FlexibleInstances, KindSignatures #-}
module Nginx.Http.Proxy (ProxyDirective, proxyDirective) where

import Data.Text
import Control.Applicative
import Data.Attoparsec.Text
import Nginx.Types (Context(..), Size, Switch)
import Nginx.Utils

data ProxyDirective (a :: Context) where
    ProxyBindDirective :: Text -> ProxyDirective a
    ProxyBufferSizeDirective :: Size -> ProxyDirective a
    ProxyBufferingDirective :: Switch -> ProxyDirective a
    ProxyBuffersDirective :: Integer -> Size -> ProxyDirective a
    ProxyBusyBuffersSizeDirective :: Size -> ProxyDirective a
    ProxyPassDirective :: Text -> ProxyDirective a
    ProxyHttpVersionDirective :: Text -> ProxyDirective a
    ProxySetHeaderDirective :: Text -> Text -> ProxyDirective a
    ProxyReadTimeoutDirective :: Integer -> ProxyDirective a
    ProxyConnectTimeoutDirective :: Integer -> ProxyDirective a
    ProxyBuffering :: Text -> ProxyDirective a

deriving instance Show (ProxyDirective a)

proxyDirective :: Parser (ProxyDirective a)
proxyDirective = proxyPass <|> proxyHttpVersion <|> proxySetHeader
                 <|> proxyReadTimeout <|> proxyConnectTimeout
                 <|> proxyBuffering

proxyPass = ProxyPassDirective <$> textValueDirective "proxy_pass"
proxyHttpVersion = ProxyHttpVersionDirective <$> textValueDirective "proxy_http_version"
proxySetHeader = uncurry ProxySetHeaderDirective <$> keyValueDirective "proxy_set_header"
proxyReadTimeout = ProxyReadTimeoutDirective <$> intValueDirective "proxy_read_timeout"
proxyConnectTimeout = ProxyConnectTimeoutDirective <$> intValueDirective "proxy_connect_timeout"
proxyBuffering = ProxyBuffering <$> textValueDirective "proxy_buffering"
