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
    ProxySendTimeoutDirective :: Integer -> ProxyDirective a
    ProxyConnectTimeoutDirective :: Integer -> ProxyDirective a
    ProxyCacheDirective :: Switch -> ProxyDirective a
    ProxyStoreDirective :: Switch -> ProxyDirective a
    ProxyCachePathDirective :: Text -> ProxyDirective a
    ProxyTempPathDirective :: Text -> ProxyDirective a
    ProxyIgnoreHeadersDirective :: Text -> Text -> ProxyDirective a
    ProxyCacheUseStaleDirective :: Text -> ProxyDirective a

deriving instance Show (ProxyDirective a)

proxyDirective :: Parser (ProxyDirective a)
proxyDirective = proxyPass <|> proxyHttpVersion <|> proxySetHeader
                 <|> proxyReadTimeout <|> proxyConnectTimeout
                 <|> proxyBuffering <|> proxyCache <|> proxyStore
                 <|> proxyCachePath <|> proxyTempPath
                 <|> proxyIgnoreHeaders <|> proxyCacheUseStale
                 <|> proxySendTimeout <|> proxyReadTimeout
                 <|> proxyConnectTimeout <|> proxyBufferSize
                 <|> proxyBuffers

proxyPass = ProxyPassDirective <$> textValueDirective "proxy_pass"
proxyHttpVersion = ProxyHttpVersionDirective <$> textValueDirective "proxy_http_version"
proxySetHeader = uncurry ProxySetHeaderDirective <$> keyValueDirective "proxy_set_header"
proxyReadTimeout = ProxyReadTimeoutDirective <$> intValueDirective "proxy_read_timeout"
proxySendTimeout = ProxySendTimeoutDirective <$> intValueDirective "proxy_send_timeout"
proxyConnectTimeout = ProxyConnectTimeoutDirective <$> intValueDirective "proxy_connect_timeout"
proxyBuffering = ProxyBufferingDirective <$> switchDirective "proxy_buffering"
proxyCache = ProxyCacheDirective <$> switchDirective "proxy_cache"
proxyStore = ProxyStoreDirective <$> switchDirective "proxy_store"
proxyCachePath = ProxyCachePathDirective <$> textValueDirective "proxy_cache_path"
proxyTempPath = ProxyTempPathDirective <$> textValueDirective "proxy_temp_path"
proxyIgnoreHeaders = (uncurry ProxyIgnoreHeadersDirective) <$> keyValueDirective "proxy_ignore_headers"
proxyCacheUseStale = ProxyCacheUseStaleDirective <$> textValueDirective "proxy_cache_use_stale"
proxyBufferSize = ProxyBufferSizeDirective <$> sizeDirective "proxy_buffer_size"
proxyBuffers = (uncurry ProxyBuffersDirective) <$> numAndSizeDirective "proxy_buffers"
