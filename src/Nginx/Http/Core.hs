{-# LANGUAGE OverloadedStrings, GADTs, StandaloneDeriving, DataKinds, FlexibleInstances, KindSignatures #-}
module Nginx.Http.Core(http,HttpDirective) where

import Prelude hiding (break)
import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Attoparsec.Text
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BS
import Text.Parser.Combinators (endBy)

import Nginx.Utils
import Nginx.Types (Context(..),Switch,Size)
import Nginx.Http.Proxy (ProxyDirective,proxyDirective)
import Nginx.Http.Gzip (GzipDirective,gzipDirective)
import Nginx.Http.Ssl (SslDirective,sslDirective)

data HttpDirective (a :: Context) where
    ServerDirective :: [HttpDirective 'ServerContext] -> HttpDirective HttpContext
    ListenDirective :: Text -> HttpDirective ServerContext
    ServerNameDirective :: [Text] -> HttpDirective ServerContext
    IfDirective :: IfCondition -> [HttpDirective 'IfContext] -> HttpDirective a
    SetDirective :: Text -> Text -> HttpDirective a
    LocationDirective :: LocationCondition -> [HttpDirective 'LocationContext] -> HttpDirective a
    InternalDirective :: HttpDirective LocationContext
    RootDirective :: Text -> HttpDirective a
    IndexDirective :: Text -> HttpDirective a
    BreakDirective :: HttpDirective a
    RewriteDirective :: Text -> HttpDirective a
    ErrorPageDirective :: [Integer] -> Text -> HttpDirective a
    AccessLogDirective :: Text -> HttpDirective a
    ExpiresDirective :: Text -> HttpDirective a
    CharsetDirective :: Text -> HttpDirective a
    DisableSymlinksDirective :: Text -> HttpDirective a
    ProxyDirective :: ProxyDirective a -> HttpDirective a
    GzipDirective :: GzipDirective a -> HttpDirective a
    SslDirective :: SslDirective a -> HttpDirective a
    IncludeDirective :: Text -> HttpDirective a
    ReturnDirective :: Text -> HttpDirective a
    UnknownDirective :: Text -> Text -> HttpDirective a
    DefaultTypeDirective :: Text -> HttpDirective a
    LogFormatDirective :: Text -> Text -> HttpDirective a
    ClientMaxBodySizeDirective :: Size -> HttpDirective a
    ClientBodyBufferSizeDirective :: Size -> HttpDirective a
    ClientHeaderTimeoutDirective :: Integer -> HttpDirective a
    ClientBodyTimeoutDirective :: Integer -> HttpDirective a
    ClientHeaderBufferSizeDirective :: Size -> HttpDirective a
    LargeClientHeaderBuffersDirective :: Integer -> Size -> HttpDirective a
    OutputBuffersDirective :: Integer -> Size -> HttpDirective a
    ServerNamesHashMaxSizeDirective :: Integer -> HttpDirective a
    ServerNamesHashBucketSizeDirective :: Integer -> HttpDirective a
    TypesHashMaxSizeDirective :: Integer -> HttpDirective a
    TypesHashBucketSizeDirective :: Integer -> HttpDirective a
    SendTimeoutDirective :: Integer -> HttpDirective a
    SendfileDirective :: Switch -> HttpDirective a
    TcpNopushDirective :: Switch -> HttpDirective a
    TcpNodelayDirective :: Switch -> HttpDirective a
    KeepaliveTimeoutDirective :: Integer -> Maybe Integer -> HttpDirective a
    SendFileDirective :: Switch -> HttpDirective a

deriving instance Show (HttpDirective a)

data IfCondition = IfVariable Text
                   | IfMatch Text MatchOp Text
                   | IfCompare Text EqOp Text
                   | IfFile Text
                   | IfDir Text
                   | IfExists Text
                   | IfExecutable Text
                   deriving (Show)

data LocationCondition = LocationUri (Maybe LocationOp) Text | LocationName Text deriving (Show)
data LocationOp = LocOpEquals | LocOpTilde | LocOpAsterisk deriving (Show)
data MatchOp = MatchOpAsterisk deriving (Show)
data EqOp = EqOpEquals | EqOpNotEquals deriving (Show)
data RewriteFlag = RewriteLast | RewriteBreak | RewriteRedirect | RewritePermanent deriving (Show)

http = httpDirective `sepBy` directiveSeparator

server = ServerDirective <$> (string "server" *> some space *> char '{' *> (serverDirective `sepBy` directiveSeparator) <* optional directiveSeparator <* char '}')

httpDirective = include <|> defaultType <|> logFormat <|> accessLog <|> clientMaxBodySize <|> clientBodyBufferSize
                <|> disableSymlinks <|> sendFile <|> tcpNopush <|> keepaliveTimeout <|> tcpNodelay
                <|> clientHeaderTimeout <|> clientBodyTimeout <|> clientHeaderBufferSize
                <|> largeClientHeaderBuffers <|> outputBuffers <|> serverNamesHashMaxSize
                <|> serverNamesHashBucketSize <|> typesHashMaxSize <|> typesHashBucketSize
                <|> sendTimeout
                <|> (ProxyDirective <$> proxyDirective)
                <|> (GzipDirective <$> gzipDirective)
                <|> (SslDirective <$> sslDirective)
                <|> server <|> index
serverDirective = listen <|> serverName <|> ifD <|> location <|> errorPage <|> accessLog
             <|> expires <|> charset <|> index
             <|> include <|> disableSymlinks <|> root
             <|> returnD
             <|> (ProxyDirective <$> proxyDirective)
             <|> (GzipDirective <$> gzipDirective)
             <|> (SslDirective <$> sslDirective)
             <|> errorPage
             <|> accessLog
locationDirective = internal <|> root <|> rewrite <|> break <|> ifD <|> errorPage <|> accessLog
             <|> expires <|> charset <|> index <|> disableSymlinks
             <|> returnD
             <|> (ProxyDirective <$> proxyDirective)
             <|> (GzipDirective <$> gzipDirective)
             <|> (SslDirective <$> sslDirective)
ifDirective = set <|> rewrite <|> break <|> errorPage <|> accessLog <|> expires <|> charset <|> index <|> returnD


include = IncludeDirective <$> textValueDirective "include"
internal = string "internal" *> char ';' *> return InternalDirective
root = RootDirective <$> (string "root" *> skipSpace *> takeWhile1 (/= ';') <* char ';')
break = string "break" *> char ';' *> return BreakDirective

-- todo
errorPage = ErrorPageDirective <$> (string "error_page" *> skipSpace *> some (decimal <* skipSpace)) <*> (optional (char '=') *> skipSpace *> (takeWhile1 (/= ';')) <* char ';')
rewrite = RewriteDirective <$> textValueDirective "rewrite" -- not really
accessLog = AccessLogDirective <$> textValueDirective "access_log"
expires = ExpiresDirective <$> textValueDirective "expires"
charset = CharsetDirective <$> textValueDirective "charset"
index = IndexDirective <$> textValueDirective "index"
disableSymlinks = DisableSymlinksDirective <$> textValueDirective "disable_symlinks"
returnD = ReturnDirective <$> textValueDirective "return"
defaultType = DefaultTypeDirective <$> textValueDirective "default_type"
logFormat = (uncurry LogFormatDirective) <$> keyValueDirective "log_format"

sendFile = SendFileDirective <$> switchDirective "sendfile"
tcpNopush = TcpNopushDirective <$> switchDirective "tcp_nopush"
tcpNodelay = TcpNodelayDirective <$> switchDirective "tcp_nodelay"
clientMaxBodySize = ClientMaxBodySizeDirective <$> sizeDirective "client_max_body_size"
clientBodyBufferSize = ClientBodyBufferSizeDirective <$> sizeDirective "client_body_buffer_size"
clientHeaderTimeout = ClientHeaderTimeoutDirective <$> intValueDirective "client_header_timeout"
clientBodyTimeout = ClientBodyTimeoutDirective <$> intValueDirective "client_body_timeout"
clientHeaderBufferSize = ClientHeaderBufferSizeDirective <$> sizeDirective "client_header_buffer_size"
largeClientHeaderBuffers = (uncurry LargeClientHeaderBuffersDirective) <$> numAndSizeDirective "large_client_header_buffers"
keepaliveTimeout = KeepaliveTimeoutDirective <$> (string "keepalive_timeout" *> skipSpace *> decimal) <*> (optional (skipSpace *> decimal)) <* char ';'
sendTimeout = SendTimeoutDirective <$> intValueDirective "send_timeout"
outputBuffers = (uncurry OutputBuffersDirective) <$> numAndSizeDirective "output_buffers"
serverNamesHashMaxSize = ServerNamesHashMaxSizeDirective <$> intValueDirective "server_names_hash_max_size"
serverNamesHashBucketSize = ServerNamesHashBucketSizeDirective <$> intValueDirective "server_names_hash_bucket_size"
typesHashMaxSize = TypesHashMaxSizeDirective <$> intValueDirective "types_hash_max_size"
typesHashBucketSize = TypesHashBucketSizeDirective <$> intValueDirective "types_hash_bucket_size"
-- never use this
unknownDirective = UnknownDirective <$> (takeWhile1 (/= ' ')) <*> (some space *> (takeWhile1 (/= ';')) <* char ';')

listen = ListenDirective <$> ((skipSpace *> string "listen" *> skipSpace *> takeWhile1 (/= ';')) <* char ';')

serverName = ServerNameDirective <$> ((skipSpace *> string "server_name" *> skipSpace *> (takeWhile1 (\c -> c /= ' ' && c /= ';')) `sepBy1` space) <* char ';')

ifD = skipSpace *> string "if" *> skipSpace *> char '(' *> skipSpace *> (ifMatch <|> ifCompare <|> ifVariable)
    where ifVariable = IfDirective <$> (IfVariable <$> variableName) <*> (char ')' *> ifBlock)
          ifCompare = IfDirective <$> (IfCompare <$> variableName <*> eqOp <*> takeWhile1 (/= ')')) <*> (char ')' *> ifBlock)
          ifMatch = do
                match <- IfMatch <$> variableName <*> matchOp
                (rest, ifb) <- followedBy anyChar (char ')' *> ifBlock)
                return $ IfDirective (match $ Text.pack rest) ifb
          ifBlock = skipSpace *> char '{' *> skipSpace *> (ifDirective `sepBy` (some space)) <* skipSpace <* char '}'
          eqOp = skipSpace *> ((char '=' *> return EqOpEquals) <|> (string "!=" *> return EqOpNotEquals)) <* skipSpace
          matchOp = skipSpace *> (string "~*" *> return MatchOpAsterisk) <* skipSpace

variableName = char '$' *> (Text.pack <$> some (letter <|> digit <|> char '_'))
set = SetDirective <$> (string "set" *> skipSpace *> variableName <* skipSpace) <*> takeWhile1 (/= ';') <* char ';' <* skipSpace

location = string "location" *> skipSpace *> (namedLocation <|> locationUri)
    where locationOp = skipSpace *> (opEquals <|> opAsterisk <|> opTilde) <* skipSpace
          opEquals = string "=" *> return LocOpEquals
          opTilde = string "~" *> return LocOpTilde
          opAsterisk = string "~*" *> return LocOpAsterisk
          locationName = LocationName <$> (char '@' *> (Text.pack <$> some (letter <|> digit <|> char '_'))) <* skipSpace
          locationUri = do
                match <- LocationUri <$> optional locationOp
                (rest, lb) <- followedBy anyChar locationBlock
                return $ LocationDirective (match $ Text.pack rest) lb
          namedLocation = LocationDirective <$> locationName <*> locationBlock
          locationBlock = skipSpace *> char '{' *> skipSpace *> (locationDirective `sepBy` (some space)) <* skipSpace <* char '}'
