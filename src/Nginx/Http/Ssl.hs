{-# LANGUAGE OverloadedStrings, GADTs, StandaloneDeriving, DataKinds, FlexibleInstances, KindSignatures #-}
module Nginx.Http.Ssl(SslDirective, sslDirective) where

import Data.Text
import Control.Applicative
import Data.Attoparsec.Text
import Nginx.Types (Context(..), Size, Switch)
import Nginx.Utils

data SslDirective (a :: Context) where
    SslCertificate :: Text -> SslDirective a
    SslCertificateKey :: Text -> SslDirective a

deriving instance Show (SslDirective a)

sslDirective :: Parser (SslDirective a)
sslDirective = sslCertificateKey <|> sslCertificate

sslCertificate = SslCertificate <$> textValueDirective "ssl_certificate"
sslCertificateKey = SslCertificateKey <$> textValueDirective "ssl_certificate_key"
