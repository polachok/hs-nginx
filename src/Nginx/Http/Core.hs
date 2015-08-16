{-# LANGUAGE OverloadedStrings, GADTs, StandaloneDeriving, DataKinds, FlexibleInstances, KindSignatures #-}
module Nginx.Http.Core(http) where

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
import Nginx.Types (Context(..))
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
    ErrorPageDirective :: Integer -> Text -> HttpDirective a
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

http = some server

server = ServerDirective <$> (string "server" *> some space *> char '{' *> (serverDirective `sepBy` (skipComment <|> skipSpace)) <* skipSpace <* char '}')

serverDirective = listen <|> serverName <|> ifD <|> location <|> errorPage <|> accessLog
             <|> expires <|> charset <|> index
             <|> include <|> disableSymlinks <|> root
             <|> returnD
             <|> (ProxyDirective <$> proxyDirective)
             <|> (GzipDirective <$> gzipDirective)
             <|> (SslDirective <$> sslDirective)
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
errorPage = ErrorPageDirective <$> (string "error_page" *> skipSpace *> decimal <* skipSpace) <*> (char '=' *> skipSpace *> (takeWhile1 (/= ';')) <* char ';')
rewrite = RewriteDirective <$> textValueDirective "rewrite" -- not really
accessLog = AccessLogDirective <$> textValueDirective "access_log"
expires = ExpiresDirective <$> textValueDirective "expires"
charset = CharsetDirective <$> textValueDirective "charset"
index = IndexDirective <$> textValueDirective "index"
disableSymlinks = DisableSymlinksDirective <$> textValueDirective "disable_symlinks"
returnD = ReturnDirective <$> textValueDirective "return"
-- proxyConnectTimeout = ProxyConnectTimeout <$> -
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
